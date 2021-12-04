{ pkgs, config, inputs, lib, ... }:

{
  imports = [
    inputs.nixos-hardware.nixosModules.lenovo-thinkpad-x1-7th-gen
    ./hardware.nix
    ../../configuration/network.nix
  ];

  # to build for pi
  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];

  networking = {
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

  # Basically just to avoid tearing
  services.picom = {
    enable = true;
    vSync = true;
    backend = "glx";
    # settings = {
    #   glx-no-stencil = true;
    #   glx-no-rebind-pixmap = true;
    #   unredir-if-possible = true;
    #   xrender-sync-fence = true;
    # };
  };

  # Prevent screen tearing
  services.xserver.videoDrivers = [ "modesetting" ];
  services.xserver.useGlamor = true;

  services.syncthing = {
    enable = true;
    #guiAddress = "localhost:8384";
    openDefaultPorts = true;
    user = "ftzm";
    configDir = "/home/ftzm/.config/syncthing";
    dataDir = "/home/ftzm";
  };

  services.fprintd.enable = true;

  system.stateVersion = "20.09";

  home-manager.users.ftzm.imports = [ ./home.nix ];
}
