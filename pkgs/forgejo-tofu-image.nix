# The Forgejo OpenTofu job's image (FORGEJO_MIGRATION_PLAN.md -> Secrets ->
# Forgejo configuration as OpenTofu): OpenTofu from nixpkgs (the official
# images stopped supporting direct use in 1.10) with what the job's script
# needs. Providers download at `tofu init`, pinned by the committed
# .terraform.lock.hcl. Streamed (uncompressed tar on stdout) for `k3s ctr
# images import -` (role/k3s-local-images.nix), which runs it on nuc.
{
  dockerTools,
  buildEnv,
  opentofu,
  bash,
  coreutils,
  curl,
  jq,
  cacert,
}:
dockerTools.streamLayeredImage {
  name = "localhost/forgejo-tofu";
  tag = "nix";
  contents = buildEnv {
    name = "forgejo-tofu-root";
    paths = [opentofu bash coreutils curl jq cacert];
    pathsToLink = ["/bin" "/etc"];
  };
  # /tmp for tofu's working copy and plugin downloads.
  extraCommands = "mkdir -m 1777 tmp";
  config = {
    Env = [
      "SSL_CERT_FILE=${cacert}/etc/ssl/certs/ca-bundle.crt"
      "HOME=/tmp"
    ];
    Entrypoint = ["/bin/bash"];
  };
}
