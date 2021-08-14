{ config, inputs, lib, ... }:

{
  imports = [
    inputs.nixos-hardware.nixosModules.lenovo-thinkpad-x1-7th-gen
    ./hardware.nix
  ];
  age.secrets.wireguard-private-key-leigheas.file =
    ../../secrets/wireguard-private-key-leigheas.age;
  networking =  {
    hostName = "leigheas"; # Define your hostname.
    useDHCP = false;
    interfaces = {
      enp0s31f6.useDHCP = true;
      wlp0s20f3.useDHCP = true;
    };
    #wireguard.interfaces = (x: abort (lib.generators.toPretty {} x)) {
    wireguard.interfaces = {
      wg0 = {
        ips = [ "10.0.100.2/24" ];
        listenPort = 51820;
        privateKeyFile = config.age.secrets.wireguard-private-key-leigheas.path;
        peers = [
          # {
          #   publicKey = "zhFBmWvFzUtyCj+eWpVrFgsgBPYwdEI2Z6wy00/fWFg=";
          #   allowedIPs = ["10.0.100.1"];
          #   endpoint = "ftzmlab.xyz:51820";
          # }

          {
            publicKey = "9742uVx1meQuUeJvlwq63I4Bc1eI5XpOjQUVUC7eP2c=";
            allowedIPs = [ "10.0.100.3" ];
            endpoint = "nas.ftzmlab.xyz:51830";
          }
          {
            publicKey = "hkBdJ/i5Aei5RXNcoYluvJcScIoDz+Na8iVhiHQv6TA=";
            allowedIPs = [ "10.0.100.4" ];
            endpoint = "nuc.ftzmlab.xyz:51840";
          }
        ];
      };
    };
    extraHosts = ''
      10.0.100.3 wg-nas
      10.0.100.4 wg-nuc
    '';
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
