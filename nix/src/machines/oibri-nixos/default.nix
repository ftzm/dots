{ inputs, ... }:

{
  imports = [
    inputs.nixos-hardware.nixosModules.lenovo-thinkpad-t480s
    ./hardware.nix
  ];
  networking.hostName = "oibri-nixos"; # Define your hostname.
  system.stateVersion = "19.03";
  home-manager.users.ftzm.imports = [ ./home.nix  ];
}
