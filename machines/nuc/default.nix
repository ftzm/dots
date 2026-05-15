{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: let
  # Beets with filetote, built via uv2nix to avoid nixpkgs beets packaging issues
  unstablePkgs = inputs.nixpkgs.legacyPackages.${pkgs.system};
  beetsWorkspace = inputs.uv2nix.lib.workspace.loadWorkspace {workspaceRoot = ./beets;};
  beetsOverlay = beetsWorkspace.mkPyprojectOverlay {sourcePreference = "wheel";};
  beetsPythonBase = unstablePkgs.callPackage inputs.pyproject-nix.build.packages {
    python = unstablePkgs.python3;
  };
  beetsPythonSet = beetsPythonBase.overrideScope (
    unstablePkgs.lib.composeManyExtensions [
      inputs.pyproject-build-systems.overlays.wheel
      beetsOverlay
      (_final: prev: {
        numba = prev.numba.overrideAttrs (old: {
          buildInputs = (old.buildInputs or []) ++ [unstablePkgs.tbb_2022];
        });
      })
    ]
  );
  beetsEnv = beetsPythonSet.mkVirtualEnv "beets-env" beetsWorkspace.deps.default;
  beetsConfig = (pkgs.formats.yaml {}).generate "beets-config.yaml" {
    directory = "/mnt/nas/music";
    library = "/home/admin/.config/beets/musiclibrary.db";
    import = {
      move = true;
      copy = false;
      languages = ["en"];
    };
    plugins = [
      "musicbrainz"
      "fromfilename"
      "badfiles"
      "permissions"
      "info"
      "fetchart"
      "discogs"
      "filetote"
      "scrub"
      "edit"
    ];
    permissions = {
      file = 664;
      dir = 775;
    };
    filetote = {
      patterns = {
        all = ["*.*[!cue]"];
      };
    };
    edit = {
      itemfields = [
        "track"
        "title"
        "album"
        "artist"
        "composer"
      ];
    };
  };
  # nginx only serves WebDAV on localhost, fronted by Traefik
  nginxListenAddrs = [
    {
      addr = "127.0.0.1";
      port = 8085;
    }
  ];
in {
  imports = [
    # Include the results of the hardware scan.
    inputs.agenix.nixosModules.age
    ./hardware.nix
    ../../role/network.nix
    ./mqtt2prometheus-service.nix
    ./k3s.nix
    ../../role/comin.nix
  ];

  # make members of wheel group trusted users, allowing them additional rights when
  # connection to nix daemon.
  # This was enable to allow deploying via deploy-rs as non-root.
  nix.settings.trusted-users = ["@wheel"];

  nix = {
    # package = pkgs.nixFlakes;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  time.timeZone = "Europe/Copenhagen";
  i18n.defaultLocale = "en_US.UTF-8";
  services.xserver.xkb.layout = "us"; # probably unnecessary on server

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  boot.kernel.sysctl."net.ipv4.ip_unprivileged_port_start" = 53;

  networking.hostName = "nuc";
  networking.useDHCP = true;
  networking.interfaces.eno1.useDHCP = true;
  networking.interfaces.wlp0s20f3.useDHCP = true;
  networking.firewall.enable = false;

  users.users.admin = {
    isNormalUser = true;
    extraGroups = ["wheel"]; # Enable ‘sudo’ for the user.
  };

  system.activationScripts.beetsConfig = ''
    mkdir -p /home/admin/.config/beets
    ln -sf ${beetsConfig} /home/admin/.config/beets/config.yaml
    chown -h admin:users /home/admin/.config/beets/config.yaml
  '';

  virtualisation.oci-containers.backend = "podman";

  environment.systemPackages = with pkgs; [
    wget
    vim
    mpc
    ncmpcpp
    beetsEnv
    sqlite
    htop
    ffmpeg
    git
    direnv
    nix-direnv
    foot.terminfo
    jellyfin
    jellyfin-web
    jellyfin-ffmpeg

    #for nzbget
    python3
  ];

  services = {
    openssh.enable = true;
    sshd.enable = true;
  };

  programs.mosh.enable = true;

  # --------------------------
  services.jellyfin = {
    enable = true;
    group = "storage";
  };

  nixpkgs.config.packageOverrides = pkgs: {
    intel-vaapi-driver = pkgs.intel-vaapi-driver.override {enableHybridCodec = true;};
  };
  hardware.graphics = {
    enable = true;
    extraPackages = with pkgs; [
      intel-media-driver
      intel-vaapi-driver
      libva-vdpau-driver
      libvdpau-va-gl
      intel-compute-runtime-legacy1
      vpl-gpu-rt
      intel-ocl
    ];
  };

  users.users.jellyfin.extraGroups = ["users" "storage"];

  # systemd.services.jellyfin.environment.LIBVA_DRIVER_NAME = "iHD"; # or i965 for older GPUs
  # environment.sessionVariables = {LIBVA_DRIVER_NAME = "iHD";};
  # --------------------------

  services.nfs.server.enable = true;
  fileSystems."/mnt/nas" = {
    device = "192.168.1.3:/pool-1";
    fsType = "nfs";
    options = ["nfsvers=3" "noatime" "nodiratime" "rsize=32768" "async" "nofail"];
  };

  services.mpd = {
    enable = true;
    musicDirectory = "/mnt/nas/music";
    network = {listenAddress = "any";};
  };

  services.prometheus.exporters.node = {
    enable = true;
    enabledCollectors = ["processes" "systemd"];
    port = 9002;
  };

  services.nzbget = {
    enable = true;
    group = "storage";
    settings = {
      MainDir = "/mnt/nas/mediastack/downloads/usenet";
    };
  };
  users.users.nzbget.extraGroups = ["users" "storage"];

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
      download_location = "/mnt/nas/mediastack/downloads/torrents";
      move_completed_path = "/mnt/nas/mediastack/downloads/torrents";
      torrentfiles_location = "/var/lib/deluge/torrents";
      move_completed = false;
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

  users.users.deluge.extraGroups = ["users" "storage"];
  systemd.services.deluged.serviceConfig.ExecStart = lib.mkForce ''
    ${pkgs.deluge}/bin/deluged --do-not-daemonize --config /var/lib/deluge/.config/deluge -L info
  '';

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

    settings = {
      global = {
        browseable = "yes";
        "smb encrypt" = "required";
      };
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

  nixpkgs.config.permittedInsecurePackages = ["openssl-1.1.1u"];
  nixpkgs.config.allowUnfree = true;

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

    settings = {
      overwriteProtocol = "https";
    };
    config = {
      # Further forces Nextcloud to use HTTPS

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

  users.groups.storage = {gid = 1001;};
  # users.users.nextcloud.extraGroups = [ "users" "storage" ];
  # users.users.nextcloud.isSystemUser = true;

  # ----------------------------------------------------------------------
  # # Librephotos

  # # we create a systemd service so that we can create a single "pod"
  # # for our containers to live inside of. This will mimic how docKER compose
  # # creates one network for the containers to live inside of
  # systemd.services.create-librephotos-network = with config.virtualisation.oci-containers; {
  #   serviceConfig.Type = "oneshot";
  #   wantedBy = [
  #     "${backend}-librephotos-backend.service"
  #     "${backend}-librephotos-db.service"
  #   ];
  #   script = ''
  #     ${pkgs.podman}/bin/podman network exists lp-net || \
  #     ${pkgs.podman}/bin/podman network create lp-net
  #   '';
  # };

  # virtualisation.oci-containers.containers.librephotos-proxy = {
  #   image = "reallibrephotos/librephotos-proxy:latest";
  #   volumes = [
  #     "/mnt/nas/cloud/photos:/data"
  #     "/librephotos/protected_media:/protected_media"
  #   ];
  #   ports = ["0.0.0.0:780:80"];
  #   extraOptions = ["--network=lp-net"];
  #   dependsOn = ["librephotos-backend" "librephotos-frontend"];
  # };

  # virtualisation.oci-containers.containers.librephotos-db = {
  #   image = "postgres:13";
  #   environment = {
  #     POSTGRES_USER = "docker";
  #     POSTGRES_PASSWORD = "AaAa1234";
  #     POSTGRES_DB = "librephotos";
  #   };
  #   volumes = ["/librephotos/data/db:/var/lib/postgresql/data"];
  #   entrypoint = "docker-entrypoint.sh";
  #   cmd = [
  #     "-c"
  #     "fsync=off"
  #     "-c"
  #     "synchronous_commit=off"
  #     "-c"
  #     "full_page_writes=off"
  #     "-c"
  #     "random_page_cost=1.0"
  #   ];
  #   extraOptions = ["--network=lp-net"];
  # };

  # virtualisation.oci-containers.containers.librephotos-frontend = {
  #   image = "reallibrephotos/librephotos-frontend:latest";
  #   extraOptions = ["--network=lp-net"];
  #   hostname = "frontend";
  # };

  # virtualisation.oci-containers.containers.librephotos-backend = {
  #   image = "reallibrephotos/librephotos:latest";
  #   volumes = [
  #     "/mnt/nas/cloud/photos:/data"
  #     "/librephotos/protected_media:/protected_media"
  #     "/librephotos/logs:/logs"
  #     "/librephotos/cache:/root/.cache"
  #   ];
  #   extraOptions = ["--network=lp-net"];
  #   hostname = "backend";
  #   environment = {
  #     SECRET_KEY = "shhhhKey";
  #     BACKEND_HOST = "backend";
  #     ADMIN_EMAIL = "";
  #     ADMIN_USERNAME = "admin";
  #     ADMIN_PASSWORD = "admin";
  #     DB_BACKEND = "postgresql";
  #     DB_NAME = "librephotos";
  #     DB_USER = "docker";
  #     DB_PASS = "AaAa1234";
  #     DB_HOST = "librephotos-db";
  #     DB_PORT = "5432";
  #     MAPBOX_API_KEY = "";
  #     WEB_CONCURRENCY = "2";
  #     SKIP_PATTERNS = "";
  #     ALLOW_UPLOAD = "false";
  #     CSRF_TRUSTED_ORIGINS = "";
  #     DEBUG = "0";
  #     HEAVYWEIGHT_PROCESS = "";
  #   };
  #   dependsOn = ["librephotos-db"];
  # };

  # ----------------------------------------------------------------------

  # force it to run as storage group so imported folders can be accessed
  #  systemd.services.photoprism.serviceConfig.Group = lib.mkForce "storage";

  virtualisation.oci-containers.containers.filestash = {
    image = "machines/filestash";
    ports = ["0.0.0.0:8334:8334"];
    environment = {};
  };

  # virtualisation.oci-containers.containers.lychee = {
  #   image = "lycheeorg/lychee";
  #   ports = ["0.0.0.0:90:80"];
  #   volumes = ["/mnt/nas/cloud/photos:/srv"];
  #   environment = {};
  # };

  # ----------------------------------------------------------------------
  # Immich

  services.immich = {
    enable = true;
    host = "0.0.0.0";
  };
  users.users.immich.extraGroups = ["users" "storage"];

  # ----------------------------------------------------------------------

  # ----------------------------------------------------------------------
  # Atuin

  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_16;
  };

  services.atuin = {
    enable = true;
    host = "0.0.0.0";
    port = 8889;
    openRegistration = true;
  };

  # ----------------------------------------------------------------------
  # Photoprism

  # start over

  # ----------------------------------------------------------------------

  # ----------------------------------------------------------------------
  services.mosquitto = {
    enable = true;
    listeners = [
      {
        acl = ["pattern readwrite #"];
        omitPasswordAuth = true;
        settings.allow_anonymous = true;
      }
    ];
  };

  # ----------------------------------------------------------------------

  # nginx: WebDAV backend only (Traefik handles routing + TLS)
  services.nginx = {
    enable = true;
    virtualHosts."dav" = {
      listen = nginxListenAddrs;
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

          auth_basic "Restricted";
          auth_basic_user_file /.htpasswd;
        '';
      };
    };
  };

  fileSystems."/var/www/dav" = {
    device = "/mnt/nas/cloud";
    options = ["bind" "nofail"];
  };
  systemd.services.nginx.serviceConfig.ReadWritePaths = ["/tmp/" "/var/www/dav/"];

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

  users.users.admin.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDjXUsGrBVN0jkm39AqfoEIG4PLxmefofNJPUtJeRnIoLZGMaS8Lw/tReVKx64+ttFWLAdkfi+djJHATxwMhhD8BwfJoP5RCz+3P97p1lQh6CjM0XrzTE9Ol6X1/D/mgS4oVa5YaVw3VszxN6Hm2BimKobvfHuIK5w/f0BoBIWxdvs0YyxCJvPsyIfmEvd8CPug9A8bo1/ni77AMpAWuw2RbEBJMk3sxHqUsHlCX/aPTjEqPusictHuy3xoHc4DSxgE/IZkV/d4wOzOUHaM+W8oKvBy8X00rMMprQ1e81WUySkh4UwgplNoD/hHGuVD0EN94ISkjwOfPGW0ACP7bVkZ"
  ];

  system.stateVersion = "24.11";

  services.tailscale.enable = true;
}
