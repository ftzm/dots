{...}: {
  imports = [./agentd-source/nix/home-manager.nix];
  services.agentd.enable = true;
}
