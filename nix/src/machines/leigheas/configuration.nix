{ pkgs, ... }:

{
  networking = {
    hostName = "leigheas"; # Define your hostname.
    useDHCP = false;
    interfaces = {
      enp0s31f6.useDHCP = true;
      wlp0s20f3.useDHCP = true;
    };
  };

  services.xserver.dpi = 192;

  system.stateVersion = "20.09";
}
