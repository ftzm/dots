{ nixos-hardware, ...}:

{
  nixos-hardware-module = nixos-hardware.nixosModules.lenovo-thinkpad-t480s;
  hardware = ./hardware.nix;
  configuration = ./t480s.nix;
  home = ./home.nix;
}
