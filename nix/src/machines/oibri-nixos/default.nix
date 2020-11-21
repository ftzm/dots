{ nixos-hardware, ...}:

{
  nixos-hardware-name = nixos-hardware.nixosModules.lenovo-thinkpad-t480s;
  hardware = ./hardware.nix;
  configuration = ./t480s.nix;
  home = ./home.nix;
}
