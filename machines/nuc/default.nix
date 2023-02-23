{ config, pkgs, inputs, ... }:

{
  imports = [ # Include the results of the hardware scan.
    inputs.agenix.nixosModules.age
    ./hardware.nix
    ../../configuration/network.nix
  ];

  # make members of wheel group trusted users, allowing them additional rights when
  # connection to nix daemon.
  # This was enable to allow deploying via deploy-rs as non-root.
  nix.trustedUsers = [ "@wheel" ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  time.timeZone = "Europe/Copenhagen";
  i18n.defaultLocale = "en_US.UTF-8";
  services.xserver.layout = "us"; # probably unnecessary on server

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.hostName = "nuc";
  networking.useDHCP = false;
  networking.interfaces.eno1.useDHCP = true;
  networking.interfaces.wlp0s20f3.useDHCP = true;
  networking.firewall.enable = false;

  users.users.admin = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  };

  environment.systemPackages = with pkgs; [
    wget
    vim
    mpc_cli
    ncmpcpp
    (beets.override { enableExtraFiles = true; })
    sqlite
    ranger
    htop
    ffmpeg
    git
    direnv
    nix-direnv
    foot.terminfo
  ];

  services = {
    openssh.enable = true;
    sshd.enable = true;
  };

  programs.mosh.enable = true;


  services.jellyfin = {
    enable = true;
    group = "storage";
  };

  users.users.jellyfin.extraGroups = [ "users" "storage" ];

  services.nfs.server.enable = true;
  fileSystems."/mnt/nas" = {
    device = "nas:/pool-1";
    fsType = "nfs";
    options = [ "nfsvers=3" "noatime" "nodiratime" "rsize=32768" "async" ];
  };

  services.mpd = {
    enable = true;
    musicDirectory = "/mnt/nas/music";
    network = { listenAddress = "any"; };
  };

  services.prometheus = {
    enable = true;
    port = 9001;
    exporters = {
      node = {
        enable = true;
        enabledCollectors = [ "processes" "systemd" ];
        port = 9002;
      };
    };
    scrapeConfigs = [{
      job_name = "node";
      static_configs = [{
        targets = [
          "127.0.0.1:${toString config.services.prometheus.exporters.node.port}"
          "10.0.0.13:9002"
        ];
      }];
    }];
  };

  services.grafana = {
    enable = true;
    domain = "grafana.ftzmlab.xyz";
    port = 2342;
    addr = "127.0.0.1";
    provision = {
      enable = true;
      datasources = [{
        name = "prometheus nuc";
        type = "prometheus";
        isDefault = true;
        url = "http://localhost:9001";
      }];
    };
  };

  services.radarr = {
    enable = true;
    group = "storage";
  };
  users.users.radarr.extraGroups = [ "users" "storage" ];

  services.sonarr = {
    enable = true;
    group = "storage";
  };
  users.users.sonarr.extraGroups = [ "users" "storage" ];

  services.prowlarr = {
    enable = true;
  };

  services.ombi = {
    enable = true;
  };


  age.secrets.deluge = {
    file = ../../secrets/deluge.age;
    owner = "deluge";
  };
  services.deluge = {
    enable = true;
    declarative = true;
    web.enable = true;
    authFile = config.age.secrets.deluge.path;
    config = {
      download_location = "/var/lib/deluge/downloads";
      move_completed_path = "/var/lib/deluge/complete";
      torrentfiles_location = "/var/lib/deluge/torrents";
      move_completed = true;
    };
    group = "storage";
  };
  # We need to make the deluge directories accessible to the storage group so
  # that other services can access finished files, etc.
  system.activationScripts = {
    delugeDirPerms = {
      text = ''
        chmod 775 -R /var/lib/deluge
        chgrp -R storage /var/lib/deluge
      '';
      deps = [];
    };
  };

  users.users.deluge.extraGroups = [ "users" "storage" ];

  # ----------------------------------------------------------------------
  # nextcloud

  services.nextcloud = {
    enable = true;
    package = pkgs.nextcloud22;
    hostName = "nextcloud.ftzmlab.xyz";

    # Use HTTPS for links
    https = true;

    home = "/mnt/nas/nextcloud";

    # Auto-update Nextcloud Apps
    autoUpdateApps.enable = true;
    # Set what time makes sense for you
    autoUpdateApps.startAt = "05:00:00";

    config = {
      # Further forces Nextcloud to use HTTPS
      overwriteProtocol = "https";

      # Nextcloud PostegreSQL database configuration, recommended over using SQLite
      dbtype = "pgsql";
      dbuser = "nextcloud";
      dbhost = "/run/postgresql"; # nextcloud will add /.s.PGSQL.5432 by itself
      dbname = "nextcloud";
      dbpassFile = config.age.secrets.nextcloud-db-pass.path;

      adminpassFile = config.age.secrets.nextcloud-admin-pass.path;
      adminuser = "admin";
    };
  };

  services.postgresql = {
    enable = true;

    # Ensure the database, user, and permissions always exist
    ensureDatabases = [ "nextcloud" ];
    ensureUsers = [{
      name = "nextcloud";
      ensurePermissions."DATABASE nextcloud" = "ALL PRIVILEGES";
    }];
  };

  systemd.services."nextcloud-setup" = {
    requires = [ "postgresql.service" ];
    after = [ "postgresql.service" ];
  };

  age.secrets.nextcloud-db-pass = {
    file = ../../secrets/nextcloud-db-pass.age;
    owner = "nextcloud";
  };

  age.secrets.nextcloud-admin-pass = {
    file = ../../secrets/nextcloud-admin-pass.age;
    owner = "nextcloud";
  };

  users.groups.storage = {
    gid = 1001;
  };
  users.users.nextcloud.extraGroups = [ "users" "storage" ];
  users.users.nextcloud.isSystemUser = true;

  # ----------------------------------------------------------------------

  security.acme = {
    acceptTerms = true;
    email = "fitz.matt.d@gmail.com";
  };

  # nginx reverse proxy
  services.nginx = {

    # Use recommended settings
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;

    # Only allow PFS-enabled ciphers with AES256
    sslCiphers = "AES256+EECDH:AES256+EDH:!aNULL";

    enable = true;
    virtualHosts.${config.services.grafana.domain} = {
      enableACME = true;
      forceSSL = true;
      locations."/" = {
        proxyPass = "http://127.0.0.1:${toString config.services.grafana.port}";
        proxyWebsockets = true;
      };
    };
    virtualHosts."deluge.ftzmlab.xyz" = {
      enableACME = true;
      forceSSL = true;
      locations."/" = {
        proxyPass =
          "http://127.0.0.1:${toString config.services.deluge.web.port}";
        proxyWebsockets = true;
      };
    };
    virtualHosts."jellyfin.ftzmlab.xyz" = {
      enableACME = true;
      forceSSL = true;
      locations."/" = {
        proxyPass = "http://127.0.0.1:8096";
        proxyWebsockets = true;
      };
    };
    virtualHosts."nextcloud.ftzmlab.xyz" = {
      forceSSL = true;
      enableACME = true;
    };
    virtualHosts."ombi.ftzmlab.xyz" = {
      forceSSL = true;
      enableACME = true;
      locations."/" = {
        proxyPass =
          "http://127.0.0.1:${toString config.services.ombi.port}";
        proxyWebsockets = true;
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
