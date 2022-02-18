{ inputs, lib, ... }:

{
  imports = [
    inputs.nixos-hardware.nixosModules.lenovo-thinkpad-t480s
    ./hardware.nix
  ];
  networking.hostName = "oibri-nixos"; # Define your hostname.
  system.stateVersion = "19.03";
  nix.settings.maxJobs = lib.mkDefault 8;
  home-manager.users.ftzm.imports = [ ./home.nix  ];
}
