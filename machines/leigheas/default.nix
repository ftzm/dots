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

  services.syncthing = {
    enable = true;
    #guiAddress = "localhost:8384";
    openDefaultPorts = true;
    user = "ftzm";
    configDir = "/home/ftzm/.config/syncthing";
    dataDir = "/home/ftzm";
    declarative = {
      overrideFolders = true;
      overrideDevices = true;
      devices = {
        nas = {
          id =
            "FWRAMNZ-PZVPLHQ-HHY3E5G-I7LRHGN-PXTVHMJ-QRL67QH-EBZY3II-UD4IKQM";
          introducer = true;
          addresses = [ "tcp://10.0.100.3" ];
        };
      };
      # folders = {
      # org = {
      # devices = [ "leigheas" "nas" ];
      # path = "/home/ftzm/org";
      # enable = true;
      # };
      # };
    };
  };

  system.stateVersion = "20.09";

  home-manager.users.ftzm.imports = [ ./home.nix ];

  # Home Manager secrets.
  # these need to be defined here to have access to agenix.

  age.secrets = {
    fitzmattd-email = {
      file = ../../secrets/fitzmattd-email.age;
      owner = "ftzm";
    };
    ftzm-org-email = {
      file = ../../secrets/ftzm-org-email.age;
      owner = "ftzm";
    };
  };
  home-manager.users.ftzm = {
    accounts.email.accounts.fitzmattd.passwordCommand =
      "${pkgs.coreutils}/bin/cat ${config.age.secrets.fitzmattd-email.path}";
    accounts.email.accounts.ftzm.passwordCommand =
      "${pkgs.coreutils}/bin/cat ${config.age.secrets.ftzm-org-email.path}";
  };
}
