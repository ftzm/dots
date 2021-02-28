{ pkgs, ... }:

{
  networking = {
    hostName = "leigheas"; # Define your hostname.
    useDHCP = false;
    interfaces = {
      enp0s31f6.useDHCP = true;
      wlp0s20f3.useDHCP = true;
    };
    wireguard.interfaces = {
      wg0 = {
        ips = ["10.0.100.2/24"];
        listenPort = 51820;
        privateKeyFile = "/home/ftzm/.wg/peer_B.key";
        peers = [
          {
            publicKey = "v8RTNAkQiwU9ON/WQQLNjjdYYTMZ/V03OmeY2prJmSg=";
            allowedIPs = ["10.0.100.1"];
            endpoint = "ftzmlab.xyz:51820";
          }
        ];
      };
    };
  };
  services.rpcbind.enable = true;

  fileSystems."/mnt/music" = {
    device = "10.0.100.1:/mnt/drive";
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
}
