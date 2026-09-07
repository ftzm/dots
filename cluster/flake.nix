{
  description = "Kubernetes GitOps development environment with Tanka and Jsonnet";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
  };

  outputs = {nixpkgs, ...}: let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
  in {
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

        # Alert-rule unit tests: promtool lives in prometheus's `cli` output,
        # not the default one (which ships only the server binary).
        prometheus.cli

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
