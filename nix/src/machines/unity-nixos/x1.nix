{ pkgs, ... }:

{
  hardware.bumblebee.enable = true;
  networking.hostName = "unity-nixos"; # Define your hostname.
}
