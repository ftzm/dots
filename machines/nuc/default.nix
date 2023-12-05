{ config, pkgs, inputs, lib, ... }:

{
  imports = [ # Include the results of the hardware scan.
    inputs.agenix.nixosModules.age
    ./hardware.nix
    ../../configuration/network.nix
  ];

  # make members of wheel group trusted users, allowing them additional rights when
  # connection to nix daemon.
  # This was enable to allow deploying via deploy-rs as non-root.
  nix.settings.trusted-users = [ "@wheel" ];

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

  virtualisation.oci-containers.backend = "podman";

  environment.systemPackages = with pkgs; [
    wget
    vim
    mpc_cli
    ncmpcpp
    beets
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
      zfs = {
        enable = true;
      };
      wireguard = {
        enable = true;
      };
    };
    scrapeConfigs = [{
      job_name = "node";
      static_configs = [{
        targets = [
          "127.0.0.1:${toString config.services.prometheus.exporters.node.port}"
        ];
      }];
    }];
  };

  services.grafana = {
    enable = true;
    settings.server.domain = "grafana.ftzmlab.xyz";
    settings.server.http_port = 2342;
    settings.server.http_addr = "127.0.0.1";
    provision = {
      enable = true;
      datasources.settings.datasources = [{
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

  services.lidarr = {
    enable = true;
    group = "storage";
  };

  services.prowlarr = { enable = true; };

  services.ombi = { enable = true; };

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
      deps = [ ];
    };
  };

  users.users.deluge.extraGroups = [ "users" "storage" ];

  services.samba-wsdd.enable =
    true; # make shares visible for windows 10 clients
  networking.firewall.allowedTCPPorts = [
    5357 # wsdd
  ];
  networking.firewall.allowedUDPPorts = [
    3702 # wsdd
  ];

  # ----------------------------------------------------------------------
  services.samba = {
    enable = true;
    extraConfig = ''
      browseable = yes
      smb encrypt = required
    '';
    shares = {
      public = {
        path = "/mnt/nas/cloud";
        browseable = "yes";
        "read only" = "no";
        #"guest ok" = "yes";
        # "create mask" = "0644";
        # "directory mask" = "0755";
        # "force user" = "admin";
        # "force group" = "storage";
      };
      # private = {
      #   path = "/mnt/Shares/Private";
      #   browseable = "yes";
      #   "read only" = "no";
      #   "guest ok" = "no";
      #   "create mask" = "0644";
      #   "directory mask" = "0755";
      #   "force user" = "username";
      #   "force group" = "groupname";
      # };
    };
  };

  # ----------------------------------------------------------------------
  # nextcloud

  nixpkgs.config.permittedInsecurePackages = [ "openssl-1.1.1u" ];

  services.nextcloud = {
    enable = false;
    package = pkgs.nextcloud27;
    hostName = "nextcloud.ftzmlab.xyz";

    # Use HTTPS for links
    https = true;

    # home = "/mnt/nas/nextcloud";

    # Auto-update Nextcloud Apps
    autoUpdateApps.enable = true;
    # Set what time makes sense for you
    autoUpdateApps.startAt = "05:00:00";

    config = {
      # Further forces Nextcloud to use HTTPS
      overwriteProtocol = "https";

      # Nextcloud PostegreSQL database configuration, recommended over using SQLite
      # dbtype = "pgsql";
      # dbuser = "nextcloud";
      # dbhost = "/run/postgresql"; # nextcloud will add /.s.PGSQL.5432 by itself
      # dbname = "nextcloud";
      # dbpassFile = config.age.secrets.nextcloud-db-pass.path;

      adminpassFile = config.age.secrets.nextcloud-admin-pass.path;
      adminuser = "admin";
    };
  };

  # services.postgresql = {
  #   enable = true;

  #   # Ensure the database, user, and permissions always exist
  #   ensureDatabases = [ "nextcloud" ];
  #   ensureUsers = [{
  #     name = "nextcloud";
  #     ensurePermissions."DATABASE nextcloud" = "ALL PRIVILEGES";
  #   }];
  # };

  # systemd.services."nextcloud-setup" = {
  #   requires = [ "postgresql.service" ];
  #   after = [ "postgresql.service" ];
  # };

  # age.secrets.nextcloud-db-pass = {
  #   file = ../../secrets/nextcloud-db-pass.age;
  #   owner = "nextcloud";
  # };

  # age.secrets.nextcloud-admin-pass = {
  #   file = ../../secrets/nextcloud-admin-pass.age;
  #   owner = "nextcloud";
  # };

  users.groups.storage = { gid = 1001; };
  # users.users.nextcloud.extraGroups = [ "users" "storage" ];
  # users.users.nextcloud.isSystemUser = true;

  # ----------------------------------------------------------------------

  # force it to run as storage group so imported folders can be accessed
  #  systemd.services.photoprism.serviceConfig.Group = lib.mkForce "storage";

  virtualisation.oci-containers.containers.filestash = {
    image = "machines/filestash";
    ports = [ "0.0.0.0:8334:8334" ];
    environment = { };
  };

  virtualisation.oci-containers.containers.filebrowser = {
    image = "filebrowser/filebrowser";
    ports = [ "0.0.0.0:8899:80" ];
    # user = "admin";
    volumes =
      [ "/mnt/nas/cloud:/srv" "/filebrowser/filebrowser_db.db:/database.db" ];
    environment = { };
  };

  virtualisation.oci-containers.containers.lychee = {
    image = "lycheeorg/lychee";
    ports = [ "0.0.0.0:90:80" ];
    volumes = [ "/mnt/nas/cloud/photos:/srv" ];
    environment = { };
  };

  # we create a systemd service so that we can create a single "pod"
  # for our containers to live inside of. This will mimic how docker compose
  # creates one network for the containers to live inside of
  systemd.services.create-photoview-network =
    with config.virtualisation.oci-containers; {
      serviceConfig.Type = "oneshot";
      wantedBy =
        [ "${backend}-photoview.service" "${backend}-photoview-db.service" ];
      script = ''
        ${pkgs.podman}/bin/podman network exists pv-net || \
        ${pkgs.podman}/bin/podman network create pv-net
      '';
    };


  virtualisation.oci-containers.containers.photoview-db = {
    image = "mysql:latest";
    volumes = [ "db:/var/lib/mysql" ];
    autoStart = true;
    environment = {
      MYSQL_DATABASE = "photoview";
      MYSQL_USER = "photoview";
      MYSQL_PASSWORD = "photosecret";
      MYSQL_RANDOM_ROOT_PASSWORD = "1";
    };
    extraOptions = [ "--network=pv-net" ];
  };

  virtualisation.oci-containers.containers.photoview = {
    image = "viktorstrate/photoview:2";
    ports = [ "0.0.0.0:8888:80" ];
    volumes = [ "/photoview:/app/cache" "/mnt/nas/cloud/photos:/photos:ro" ];
    extraOptions = [ "--network=pv-net" ];
    environment = {
      PHOTOVIEW_DATABASE_DRIVER = "mysql";
      PHOTOVIEW_MYSQL_URL = "photoview:photosecret@tcp(photoview-db)/photoview";
      PHOTOVIEW_LISTEN_IP = "photoview";
      PHOTOVIEW_LISTEN_PORT = "80";
      PHOTOVIEW_MEDIA_CACHE = "/app/cache";
    };
  };

  # ----------------------------------------------------------------------

  security.acme = {
    acceptTerms = true;
    defaults.email = "fitz.matt.d@gmail.com";
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
    virtualHosts.${config.services.grafana.settings.server.domain} = {
      enableACME = true;
      forceSSL = true;
      locations."/" = {
        proxyPass = "http://127.0.0.1:${
            toString config.services.grafana.settings.server.http_port
          }";
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
    virtualHosts."ombi.ftzmlab.xyz" = {
      forceSSL = true;
      enableACME = true;
      locations."/" = {
        proxyPass = "http://127.0.0.1:${toString config.services.ombi.port}";
        proxyWebsockets = true;
      };
    };
    virtualHosts."img.ftzmlab.xyz" = {
      enableACME = true;
      forceSSL = true;
      locations."/" = {
        proxyPass = "http://127.0.0.1:8888";
        proxyWebsockets = true;
      };
    };
    virtualHosts."filestash.ftzmlab.xyz" = {
      enableACME = true;
      forceSSL = true;
      locations."/" = {
        proxyPass = "http://127.0.0.1:8334";
        proxyWebsockets = true;
      };
    };
    virtualHosts."dav.ftzmlab.xyz" = {
      enableACME = true;
      forceSSL = true;
      root = "/var/www/dav/";
      locations."/" = {
        extraConfig = ''
          autoindex on;

          dav_methods PUT DELETE MKCOL COPY MOVE;
          dav_ext_methods PROPFIND OPTIONS;
          dav_access user:rw group:rw all:rw;

          client_max_body_size 0;
          create_full_put_path on;
          client_body_temp_path /tmp/;

          #auth_pam "Restricted";
          #auth_pam_service_name "common-auth";
          auth_basic "Restricted";
          auth_basic_user_file /.htpasswd;

        '';
      };
    };
  };

  fileSystems."/var/www/dav" = {
    device = "/mnt/nas/cloud";
    options = [ "bind" ];
  };
  systemd.services.nginx.serviceConfig.ReadWritePaths =
    [ "/tmp/" "/var/www/dav/" ];

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
