{ config, pkgs, inputs, lib, ... }:

with lib;
let shares = [ "music" ];

in {
  imports = [ # Include the results of the hardware scan.
    inputs.agenix.nixosModules.age
    ./hardware.nix
    ../../network.nix
  ];

  # make members of wheel group trusted users, allowing them additional rights when
  # connection to nix daemon.
  # This was enable to allow deploying via deploy-rs as non-root.
  nix.trustedUsers = [ "@wheel" ];

  boot.loader.systemd-boot.enable = true;

  # For ZFS
  # ZFS needs this unique id
  networking.hostId = "b901a7b2";
  boot.supportedFilesystems = [ "zfs" ];
  services.zfs.zed.settings = {
    ZED_EMAIL_ADDR = [ "m@ftzm.org" ];
    ZED_DEBUG_LOG = "/tmp/zed.debug.log";
    ZED_EMAIL_OPTS = "-s '@SUBJECT@' @ADDRESS@";
    ZED_NOTIFY_INTERVAL_SECS = 3600;
    # verbose to send messages on scrubs with no errors
    ZED_NOTIFY_VERBOSE = false;
  };

  time.timeZone = "Europe/Copenhagen";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.hostName = "nas";
  networking.interfaces.eno1.useDHCP = true;

  users.users.admin = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  };

  environment.systemPackages = with pkgs; [
    wget
    vim
    lshw
    smartmontools
    hdparm
    ranger
    htop
  ];

  age.secrets.smtppw.file =
    ../../secrets/smtppw.age;

  # Enable the OpenSSH daemon.
  services = {
    sshd.enable = true;
    openssh.enable = true;
    ssmtp = {
      enable = true;
      setSendmail = true;
      root = "m@ftzm.org";
      useTLS = true;
      hostName = "smtp.fastmail.com:465";
      domain = "ftzmlab.xyz";
      authUser = "ftzm@fastmail.com";
      authPassFile = config.age.secrets.smtppw.path;
      settings = {
        # Debug = "true";
      };
    };
    smartd = {
      enable = true;
      notifications = {
        # Nice to temporarily enable when testing a new configuration
        # test = true;
        mail = {
          enable = true;
          sender = "nas@ftzmlab.xyz";
          recipient = "m@ftzm.org";
        };
      };
      autodetect = false;
      devices = [
        { device = "/dev/sda"; }
        { device = "/dev/sdb"; }
        { device = "/dev/sdc"; }
        { device = "/dev/sdd"; }
        { device = "/dev/sde"; }
      ];
      defaults.monitored = "-a -o on -s (S/../.././02|L/../../7/04)";
    };
    nfs.server = {
      enable = true;
      exports = ''
        /pool-1/ *(rw,fsid=root,no_subtree_check)
        ${concatMapStringsSep "\n" (n: "/pool-1/${n} *(rw,no_subtree_check,nohide)") shares}
      '';
    };
  };

  networking.firewall.enable = false;

  services.prometheus = {
    #enable = true;
    port = 9001;
    exporters = {
      node = {
        enable = true;
        enabledCollectors = [ "process" "systemd" ];
        port = 9002;
      };
    };
  };

  # Not ideal, but makes deployment smoother
  security.sudo.extraRules = [{
    groups = [ "wheel" ];
    commands = [{
      command = "ALL";
      options = [ "NOPASSWD" ];
    }];
  }];

  users.users.admin.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDjXUsGrBVN0jkm39AqfoEIG4PLxmefofNJPUtJeRnIoLZGMaS8Lw/tReVKx64+ttFWLAdkfi+djJHATxwMhhD8BwfJoP5RCz+3P97p1lQh6CjM0XrzTE9Ol6X1/D/mgS4oVa5YaVw3VszxN6Hm2BimKobvfHuIK5w/f0BoBIWxdvs0YyxCJvPsyIfmEvd8CPug9A8bo1/ni77AMpAWuw2RbEBJMk3sxHqUsHlCX/aPTjEqPusictHuy3xoHc4DSxgE/IZkV/d4wOzOUHaM+W8oKvBy8X00rMMprQ1e81WUySkh4UwgplNoD/hHGuVD0EN94ISkjwOfPGW0ACP7bVkZ"
  ];

  system.stateVersion = "20.09";

}
