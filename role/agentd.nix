# Vendored source keeps deployments independent of local checkout paths.
{...}: {
  imports = [./agentd-source/nix/nixos.nix];
  services.agentd.enable = true;
}
