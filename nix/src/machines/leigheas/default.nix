{ nixos-hardware, ...}:

{
  nixos-hardware-module = nixos-hardware.nixosModules.lenovo-thinkpad-x1-7th-gen;
  hardware = ./hardware.nix;
  configuration = ./configuration.nix;
  home = ./home.nix;
}
