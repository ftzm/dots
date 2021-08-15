{ config, inputs, lib, ... }:

{
  imports = [
    inputs.nixos-hardware.nixosModules.lenovo-thinkpad-x1-7th-gen
    ./hardware.nix
    ../../network.nix
  ];
  networking =  {
    hostName = "leigheas"; # Define your hostname.
    useDHCP = false;
    interfaces = {
      enp0s31f6.useDHCP = true;
      wlp0s20f3.useDHCP = true;
    };
  };
  services.rpcbind.enable = true;

  fileSystems."/mnt/music" = {
    device = "10.0.100.3:/pool-1/music";
    fsType = "nfs";
    options = [
      "nfsvers=3"
      "x-systemd.automount"
      "noauto"
      "noatime"
      "nodiratime"
      "rsize=32768"
      "async"
      "ro"
    ];
  };

  services.xserver.dpi = 192;
  hardware.video.hidpi.enable = true;
  services.autorandr.enable = true;

  system.stateVersion = "20.09";
  home-manager.users.ftzm.imports = [ ./home.nix ];
}
