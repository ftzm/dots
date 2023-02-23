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
  # networking.networkmanager.insertNameservers = ["10.2.34.189"];
  # networking.resolvconf.extraConfig = ''
  #   name_servers="10.2.35.177 10.2.34.189"
  # '';
  # networking.resolvconf.enable = false;
  networking.nameservers = ["1.1.1.1"];

  services.rpcbind.enable = true;

  services.printing.enable = true;
  services.avahi.enable = true;
  # Important to resolve .local domains of printers, otherwise you get an error
  # like  "Impossible to connect to XXX.local: Name or service not known"
  services.avahi.nssmdns = true;

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

  services.syncthing = {
    enable = true;
    #guiAddress = "localhost:8384";
    openDefaultPorts = true;
    user = "ftzm";
    configDir = "/home/ftzm/.config/syncthing";
    dataDir = "/home/ftzm";
  };

  services.xserver.dpi = 192;

  programs.steam = { enable = true; };

  system.stateVersion = "20.09";

  home-manager.users.ftzm.imports = [ ./home.nix ];
}
