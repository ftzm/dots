{ pkgs, ... }:

{
  networking.hostName = "leigheas"; # Define your hostname.
  networking.useDHCP = false;
  networking.interfaces.enp0s31f6.useDHCP = true;
  networking.interfaces.wlp0s20f3.useDHCP = true;
  system.stateVersion = "20.09";

  services.xserver.dpi = 192;
}
