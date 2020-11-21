{ nixos-hardware, ...}:

{
  nixos-hardware-module = nixos-hardware.nixosModules.lenovo-thinkpad-x1-extreme;
  hardware = ./hardware.nix;
  configuration = ./x1.nix;
  home = ./home.nix;
}
