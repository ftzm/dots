{
  config,
  pkgs,
  inputs,
  lib,
  ...
}:
with lib; let
  shares = ["music"];
in {
  imports = [
    # Include the results of the hardware scan.
    inputs.agenix.nixosModules.age
    ./hardware.nix
    ../../configuration/network.nix
  ];

  # make members of wheel group trusted users, allowing them additional rights when
  # connection to nix daemon.
  # This was enable to allow deploying via deploy-rs as non-root.
  nix.settings.trusted-users = ["@wheel"];

  users.groups.storage = {gid = 1001;};
  users.users.root.extraGroups = ["users" "storage"];

  boot.loader.systemd-boot.enable = true;

  # For ZFS
  # ZFS needs this unique id
  networking.hostId = "b901a7b2";
  boot.supportedFilesystems = ["zfs"];
  services.zfs.zed.settings = {
    ZED_EMAIL_ADDR = ["m@ftzm.org"];
    ZED_DEBUG_LOG = "/tmp/zed.debug.log";
    ZED_EMAIL_OPTS = "-s '@SUBJECT@' @ADDRESS@";
    ZED_NOTIFY_INTERVAL_SECS = 3600;
    # verbose to send messages on scrubs with no errors
    ZED_NOTIFY_VERBOSE = false;
  };
  services.zfs.autoScrub = {
    enable = true;
    interval = "Mon, 02:00";
  };

  # Mount the dang thing
  fileSystems."/pool-1" = {
    device = "pool-1";
    fsType = "zfs";
  };

  time.timeZone = "Europe/Copenhagen";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.hostName = "nas";
  networking.interfaces.eno1.useDHCP = true;

  environment.systemPackages = with pkgs; [
    wget
    vim
    lshw
    smartmontools
    hdparm
    htop
    shntool
    cuetools
    flac
    foot.terminfo
  ];

  age.secrets.smtppw.file = ../../secrets/smtppw.age;

  # Set up email for sending mail from the server (e.g. reporting on
  # automated tasks or alerts)
  programs = {
    msmtp = {
      enable = true;
      setSendmail = true;
      accounts = {
        default = {
          tls = true;
          tls_starttls = true;
          # tls_starttls = false;
          auth = true;
          from = "nas@ftzmlab.xyz";
          host = "smtp.fastmail.com";
          port = 587; # for starttls
          #port = 565;
          user = "ftzm@fastmail.com";
          passwordeval = "${pkgs.coreutils}/bin/cat ${config.age.secrets.smtppw.path}";
        };
      };
    };
  };

  services = {
    sshd.enable = true;
    openssh.enable = true;
    smartd = {
      enable = true;
      notifications = {
        # Nice to temporarily enable when testing a new configuration
        test = true;
        mail = {
          enable = true;
          sender = "nas@ftzmlab.xyz";
          recipient = "m@ftzm.org";
        };
      };
      autodetect = false;
      devices = [
        {device = "/dev/sda";}
        {device = "/dev/sdb";}
        {device = "/dev/sdc";}
        {device = "/dev/sdd";}
        {device = "/dev/sde";}
      ];
      defaults.monitored = "-a -o on -s (S/../.././02|L/../../7/04)";
    };
    nfs.server = {
      enable = true;
      exports = ''
        /pool-1/ *(rw,fsid=root,no_subtree_check,no_root_squash)
        ${concatMapStringsSep "\n"
          (n: "/pool-1/${n} *(rw,no_subtree_check,nohide,no_root_squash)")
          shares}
      '';
    };
  };

  #############################################################################
  # Borg Backup

  age.secrets.borgbase-key.file = ../../secrets/borgbase_key.age;
  age.secrets.borgbase-passphrase.file = ../../secrets/borgbase_passphrase.age;

  services.borgbackup.jobs."borgbase" = {
    paths = ["/pool-1/cloud/photos"];
    exclude = [];
    repo = "d6hr008k@d6hr008k.repo.borgbase.com:repo";
    encryption = {
      mode = "repokey-blake2";
      passCommand = "${pkgs.coreutils}/bin/cat ${config.age.secrets.borgbase-passphrase.path}";
    };
    environment.BORG_RSH = "ssh -i ${config.age.secrets.borgbase-key.path}";
    compression = "auto,lzma";
    startAt = "daily";
  };

  #############################################################################

  networking.firewall.enable = false;

  services.prometheus = {
    # we don't need prometheus proper, just the exporter
    #enable = true;
    port = 9001;
    exporters = {
      node = {
        enable = true;
        enabledCollectors = ["processes" "systemd"];
        port = 9002;
      };
    };
  };

  services.syncthing = {
    enable = true;
    guiAddress = "http://0.0.0.0:8384";
    openDefaultPorts = true;
    user = "admin";
    configDir = "/home/admin/.config/syncthing";
    dataDir = "/home/admin/sync";
    # I think these mean it doesn't try to merge the configs, and the merging is error prone.
    # overrideFolders = true;
    # overrideDevices = true;
    # devices = {
    #   leigheas.id = "3QY6BVK-DLFFWP5-WT62MGS-7WX3NQ5-X5BNZDA-ZRG44DM-DDH7LPQ-EMU4BQN";
    #   oibri-nixos.id = "XKUQLBZ-YZZ2OTU-TDBLNFK-CKUKTAH-5Q4JUIK-6G4K5WP-EHVJFBX-SN5JRAQ";
    #   phone.id = "PXRZLWU-5SGAHJC-5ZOID7T-ZNRZG32-6HWJKDG-PRYTIBS-WZSXNAE-HEIFSAZ";
    #   nas.id = "FWRAMNZ-PZVPLHQ-HHY3E5G-I7LRHGN-PXTVHMJ-QRL67QH-EBZY3II-UD4IKQM";
    #   saoiste.id = "72USTHU-DTF5LZP-TPF5URJ-NNYSJW5-JFVNQQW-KKQHJHY-KL7ZCAZ-NC26SQP";
    #   eachtrai.id = "MVEJLRV-Y2K6WNZ-SFGOKM6-XMIPA4M-FSJRFVM-6LHQBK4-B2RRWMN-YMFL4QA";
    # };
    # folders = {
    #   org = {
    #     devices = ["leigheas" "nas" "saoiste" "eachtrai"];
    #     path = "/pool-1/org";
    #     enable = true;
    #   };
    #   password-store = {
    #     devices = ["leigheas" "nas" "saoiste" "eachtrai"];
    #     path = "/pool-1/.password-store";
    #     enable = true;
    #   };
    # };
  };

  # Not ideal, but makes deployment smoother
  security.sudo.extraRules = [
    {
      groups = ["wheel"];
      commands = [
        {
          command = "ALL";
          options = ["NOPASSWD"];
        }
      ];
    }
  ];

  users.users.admin = {
    isNormalUser = true;
    extraGroups = ["wheel" "users" "storage"];
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDjXUsGrBVN0jkm39AqfoEIG4PLxmefofNJPUtJeRnIoLZGMaS8Lw/tReVKx64+ttFWLAdkfi+djJHATxwMhhD8BwfJoP5RCz+3P97p1lQh6CjM0XrzTE9Ol6X1/D/mgS4oVa5YaVw3VszxN6Hm2BimKobvfHuIK5w/f0BoBIWxdvs0YyxCJvPsyIfmEvd8CPug9A8bo1/ni77AMpAWuw2RbEBJMk3sxHqUsHlCX/aPTjEqPusictHuy3xoHc4DSxgE/IZkV/d4wOzOUHaM+W8oKvBy8X00rMMprQ1e81WUySkh4UwgplNoD/hHGuVD0EN94ISkjwOfPGW0ACP7bVkZ"
    ];
  };

  system.stateVersion = "20.09";
}
