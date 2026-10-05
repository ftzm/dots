{
  description = "Kubernetes GitOps development environment with Tanka and Jsonnet";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
  };

  outputs = {
    self,
    nixpkgs,
    ...
  }: let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};

    # The render's input: everything tk reads, without its own previous
    # output (manifests/) or the tests, so neither invalidates it.
    src = builtins.path {
      name = "cluster-src";
      path = self;
      filter = path: _: let
        rel = pkgs.lib.removePrefix (toString self + "/") (toString path);
      in
        !(builtins.elem rel ["manifests" "tests" "flake.lock"]);
    };

    # `just render-lab` in the nix sandbox: no network, no host files, the
    # same tanka/jsonnet/helm as the dev shell. The output is the full
    # manifests/ tree -- one directory per namespace (one ArgoCD Application
    # each), each SopsSecret copied into its namespace's directory.
    renderLab = pkgs.runCommand "render-lab" {nativeBuildInputs = with pkgs; [tanka kubernetes-helm gawk];} ''
      export HOME=$TMPDIR
      cp -r ${src} src
      chmod -R u+w src
      cd src
      tk export "$out" environments/lab --recursive \
        --format '{{env.metadata.name}}/{{.metadata.name}}-{{.kind | lower}}' --skip-manifest
      for f in environments/lab/secrets/*.enc.yaml; do
        ns=$(awk '/^metadata:/ {m=1; next} m && /^[^ ]/ {m=0} m && $1 == "namespace:" {print $2; exit}' "$f")
        if [ -z "$ns" ] || [ ! -d "$out/$ns" ]; then
          echo "render-lab: $f: namespace '$ns' has no environment" >&2
          exit 1
        fi
        cp "$f" "$out/$ns"/
      done
    '';
  in {
    packages.${system}.render-lab = renderLab;

    checks.${system} = {
      # The committed manifests/ are what ArgoCD deploys; they must be exactly
      # what the jsonnet renders.
      render-lab = pkgs.runCommand "render-lab-check" {} ''
        if ! diff -r ${renderLab} ${self}/manifests > diff.txt; then
          echo "cluster/manifests differs from the render; run 'just render-lab' and commit:" >&2
          head -200 diff.txt >&2
          exit 1
        fi
        touch $out
      '';

      # `just test-rules` on the render: promtool's unit tests on the
      # PrometheusRules, lokitool's parse of the Loki rules.
      test-rules = pkgs.runCommand "test-rules" {nativeBuildInputs = with pkgs; [prometheus.cli grafana-loki yq-go];} ''
        cp -r ${self}/tests tests
        chmod -R u+w tests
        mkdir -p tests/.rules
        for f in ${renderLab}/*/*-prometheusrule.yaml; do
          yq '.spec' "$f" > "tests/.rules/$(basename "$f")"
        done
        promtool test rules tests/*.test.yaml
        for f in ${renderLab}/*/loki-rule-*-configmap.yaml; do
          rules="tests/.rules/$(basename "$f")"
          yq -r '.data | to_entries | .[0].value' "$f" > "$rules"
          lokitool rules check --rule-files="$rules"
        done
        touch $out
      '';
    };

    devShells.${system}.default = pkgs.mkShell {
      packages = with pkgs; [
        bashInteractive

        # Kubernetes tools
        kubectl
        kubernetes-helm
        kustomize
        k9s

        # Jsonnet ecosystem
        jsonnet
        jsonnet-bundler
        go-jsonnet

        # Grafana Tanka
        tanka

        # Alert-rule tests: promtool lives in prometheus's `cli` output, not
        # the default one (which ships only the server binary). lokitool
        # parses the LogQL rules, which promtool cannot read.
        prometheus.cli
        grafana-loki

        # Utilities
        yq-go
        jq
        just
        renovate

        # Secrets management
        sops
        age
        kubeseal
        gitleaks
      ];
    };
  };
}
