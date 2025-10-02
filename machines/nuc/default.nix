{
  config,
  pkgs,
  inputs,
  ...
}: {
  imports = [
    # Include the results of the hardware scan.
    inputs.agenix.nixosModules.age
    ./hardware.nix
    ../../role/network.nix
    ./mqtt2prometheus-service.nix
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
  networking.hostName = "nuc";
  networking.useDHCP = true;
  networking.interfaces.eno1.useDHCP = true;
  networking.interfaces.wlp0s20f3.useDHCP = true;
  networking.firewall.enable = false;

  users.users.admin = {
    isNormalUser = true;
    extraGroups = ["wheel"]; # Enable ‘sudo’ for the user.
  };

  virtualisation.oci-containers.backend = "podman";

  environment.systemPackages = with pkgs; [
    wget
    vim
    mpc
    ncmpcpp
    # error building
    # (beets.override {
    #   pluginOverrides = {
    #     extrafiles = {
    #       enable = true;
    #       propagatedBuildInputs = [beetsPackages.extrafiles];
    #     };
    #   };
    # })
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
      # intel-vaapi-driver
      # libva-vdpau-driver
      # libvdpau-va-gl
      intel-compute-runtime-legacy1
    ];
  };

  users.users.jellyfin.extraGroups = ["users" "storage"];

  systemd.services.jellyfin.environment.LIBVA_DRIVER_NAME = "iHD"; # or i965 for older GPUs
  environment.sessionVariables = {LIBVA_DRIVER_NAME = "iHD";};
  # --------------------------

  services.nfs.server.enable = true;
  fileSystems."/mnt/nas" = {
    device = "nas:/pool-1";
    fsType = "nfs";
    options = ["nfsvers=3" "noatime" "nodiratime" "rsize=32768" "async"];
  };

  services.mpd = {
    enable = true;
    musicDirectory = "/mnt/nas/music";
    network = {listenAddress = "any";};
  };

  services.prometheus = {
    enable = true;
    port = 9001;
    exporters = {
      node = {
        enable = true;
        enabledCollectors = ["processes" "systemd"];
        port = 9002;
      };
      zfs = {enable = true;};
      wireguard = {enable = true;};
    };
    scrapeConfigs = [
      {
        job_name = "node";
        static_configs = [
          {
            targets = [
              "127.0.0.1:${toString config.services.prometheus.exporters.node.port}"
              "nas:9002"
              "127.0.0.1:9641" # mqtt2prometheus
            ];
          }
        ];
      }
    ];
  };

  services.loki = {
    enable = true;
    configuration = {
      server.http_listen_port = 3030;
      auth_enabled = false;

      ingester = {
        lifecycler = {
          address = "127.0.0.1";
          ring = {
            kvstore = {
              store = "inmemory";
            };
            replication_factor = 1;
          };
        };
        chunk_idle_period = "1h";
        max_chunk_age = "1h";
        chunk_target_size = 999999;
        chunk_retain_period = "30s";
      };

      schema_config = {
        configs = [
          {
            from = "2023-01-05";
            store = "tsdb";
            object_store = "filesystem";
            schema = "v13";
            index = {
              period = "24h";
              prefix = "index_";
            };
          }
        ];
      };

      storage_config = {
        tsdb_shipper = {
          active_index_directory = "/var/lib/loki/tsdb-index";
          cache_location = "/var/lib/loki/tsdb-cache";
          cache_ttl = "24h";
        };

        filesystem = {
          directory = "/var/lib/loki/chunks";
        };
      };

      # for tsdb
      query_scheduler = {
        max_outstanding_requests_per_tenant = 32768;
      };
      # for tsdb
      querier = {
        max_concurrent = 16;
      };

      limits_config = {
        reject_old_samples = true;
        reject_old_samples_max_age = "168h";
      };

      table_manager = {
        retention_deletes_enabled = false;
        retention_period = "0s";
      };

      compactor = {
        working_directory = "/var/lib/loki";
        compactor_ring = {
          kvstore = {
            store = "inmemory";
          };
        };
      };
    };
    # user, group, dataDir, extraFlags, (configFile)
  };

  services.promtail = {
    enable = true;
    configuration = {
      server = {
        http_listen_port = 3031;
        grpc_listen_port = 0;
      };
      positions = {
        filename = "/tmp/positions.yaml";
      };
      clients = [
        {
          url = "http://127.0.0.1:${toString config.services.loki.configuration.server.http_listen_port}/loki/api/v1/push";
        }
      ];
      scrape_configs = [
        {
          job_name = "nginx";
          static_configs = [
            {
              labels = {
                job = "nginx";
                host = "nuc";
                agent = "promtail";
                __path__ = "/var/log/nginx/access.log";
              };
            }
          ];
          pipeline_stages = [
            {
              json = {
                expressions = {
                  timestamp = "timestamp";
                };
              };
            }
            {
              timestamp = {
                source = "timestamp";
                format = "RFC3339Nano";
              };
            }
          ];
        }
        {
          job_name = "journal";
          journal = {
            max_age = "12h";
            labels = {
              job = "systemd-journal";
              host = "nuc";
            };
          };
          relabel_configs = [
            {
              source_labels = ["__journal__systemd_unit"];
              target_label = "unit";
            }
            {
              source_labels = ["__journal_priority_keyword"];
              target_label = "level";
            }
            {
              source_labels = ["__journal_syslog_identifier"];
              target_label = "syslog_identifier";
            }
          ];
          pipeline_stages = [
            {
              match = {
                selector = "{unit=\"promtail.service\"}";
                action = "drop";
              };
            }
            {
              match = {
                selector = "{syslog_identifier=\"podman\"}";
                action = "drop";
              };
            }
          ];
        }
      ];
    };
  };
  # need to make promtail user a member of the nginx group so that it can access log files
  users.users.promtail.extraGroups = ["nginx"];

  services.grafana = {
    enable = true;
    settings.server.domain = "grafana.ftzmlab.xyz";
    settings.server.http_port = 2342;
    settings.server.http_addr = "127.0.0.1";
    provision = {
      enable = true;
      datasources.settings.datasources = [
        {
          name = "prometheus nuc";
          type = "prometheus";
          isDefault = true;
          url = "http://localhost:9001";
        }
        {
          name = "Loki";
          type = "loki";
          access = "proxy";
          url = "http://127.0.0.1:${toString config.services.loki.configuration.server.http_listen_port}";
        }
      ];
    };
  };

  services.radarr = {
    enable = true;
    group = "storage";
    settings.update.automatically = true;
  };
  users.users.radarr.extraGroups = ["users" "storage"];

  services.sonarr = {
    enable = true;
    group = "storage";
    settings.update.automatically = true;
  };
  users.users.sonarr.extraGroups = ["users" "storage"];

  services.lidarr = {
    enable = true;
    group = "storage";
    settings.update.automatically = true;
  };

  services.flaresolverr = {
    enable = true;
  };

  services.prowlarr = {
    enable = true;
    settings.update.automatically = true;
  };

  services.readarr = {
    enable = true;
    group = "storage";
    settings.update.automatically = true;
  };

  services.audiobookshelf = {
    enable = true;
    group = "storage";
    host = "0.0.0.0";
  };

  services.ombi = {enable = true;};

  services.vaultwarden = {
    enable = true;
    backupDir = "/vaultwarden-backup";
    environmentFile = config.age.secrets.vaultwarden-env.path;
    config = {
      DOMAIN = "https://vaultwarden.ftzmlab.xyz";
      ROCKET_ADDRESS = "127.0.0.1";
      ROCKET_PORT = 8222;
      DATA_FOLDER = "/var/lib/vaultwarden";
    };
  };

  age.secrets.deluge = {
    file = ../../secrets/deluge.age;
    owner = "deluge";
  };

  age.secrets.vaultwarden-env = {
    file = ../../secrets/vaultwarden.age;
    path = "/var/lib/vaultwarden/.env";
    owner = "vaultwarden";
    group = "vaultwarden";
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

  users.users.deluge.extraGroups = ["users" "storage"];

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

  virtualisation.oci-containers.containers.filebrowser = {
    image = "filebrowser/filebrowser";
    ports = ["0.0.0.0:8899:80"];
    # user = "admin";
    volumes = ["/mnt/nas/cloud:/srv" "/filebrowser/filebrowser_db.db:/database.db"];
    environment = {};
  };

  # virtualisation.oci-containers.containers.lychee = {
  #   image = "lycheeorg/lychee";
  #   ports = ["0.0.0.0:90:80"];
  #   volumes = ["/mnt/nas/cloud/photos:/srv"];
  #   environment = {};
  # };

  # we create a systemd service so that we can create a single "pod"
  # for our containers to live inside of. This will mimic how docker compose
  # creates one network for the containers to live inside of
  systemd.services.create-photoview-network = with config.virtualisation.oci-containers; {
    serviceConfig.Type = "oneshot";
    wantedBy = ["${backend}-photoview.service" "${backend}-photoview-db.service"];
    script = ''
      ${pkgs.podman}/bin/podman network exists pv-net || \
      ${pkgs.podman}/bin/podman network create pv-net
    '';
  };

  virtualisation.oci-containers.containers.photoview-db = {
    image = "mysql:latest";
    volumes = ["db:/var/lib/mysql"];
    autoStart = true;
    environment = {
      MYSQL_DATABASE = "photoview";
      MYSQL_USER = "photoview";
      MYSQL_PASSWORD = "photosecret";
      MYSQL_RANDOM_ROOT_PASSWORD = "1";
    };
    extraOptions = ["--network=pv-net"];
  };

  virtualisation.oci-containers.containers.photoview = {
    image = "viktorstrate/photoview:2";
    ports = ["0.0.0.0:8888:80"];
    volumes = ["/photoview:/app/cache" "/mnt/nas/cloud/photos:/photos:ro"];
    extraOptions = ["--network=pv-net"];
    environment = {
      PHOTOVIEW_DATABASE_DRIVER = "mysql";
      PHOTOVIEW_MYSQL_URL = "photoview:photosecret@tcp(photoview-db)/photoview";
      PHOTOVIEW_LISTEN_IP = "photoview";
      PHOTOVIEW_LISTEN_PORT = "80";
      PHOTOVIEW_MEDIA_CACHE = "/app/cache";
    };
  };

  # ----------------------------------------------------------------------
  # Immich

  services.immich = {
    enable = true;
    host = "0.0.0.0";
  };
  users.users.immich.extraGroups = ["users" "storage"];

  # ----------------------------------------------------------------------

  virtualisation.oci-containers.containers.pigallery2 = {
    image = "bpatrik/pigallery2:latest";
    environment = {
      NODE_ENV = "production"; # set to 'debug' for full debug logging
    };
    volumes = [
      "/pigallery2/config:/app/data/config" # CHANGE ME
      "db-data:/app/data/db"
      "/mnt/nas/cloud/photos:/app/data/images:ro" # CHANGE ME, ':ro' means read-only
      "/pigallery2/tmp:/app/data/tmp" # CHANGE ME
    ];
    ports = ["0.0.0.0:8875:80"];
  };

  # ----------------------------------------------------------------------
  # Vaultwarden

  services.vaultwarden = {
    enable = true;
    config = {
      ROCKET_ADDRESS = "127.0.0.1";
      ROCKET_PORT = 8222;
      DOMAIN = "https://vault.ftzmlab.xyz";
    };
  };

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
  # The Lounge

  services.thelounge = {
    enable = true;
  };

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

  security.acme = {
    acceptTerms = true;
    defaults.email = "fitz.matt.d@gmail.com";
  };

  # nginx reverse proxy
  services.nginx = {
    commonHttpConfig = ''
      map "$time_iso8601 # $msec" $time_iso8601_ms { "~(^[^+]+)(\+[0-9:]+) # \d+\.(\d+)$" $1.$3$2; }

      log_format main_json escape=json '{'
        '"timestamp":"$time_iso8601_ms",'
        '"remote_addr":"$remote_addr",'
        '"remote_user":"$remote_user",'
        '"host":"$host",'
        '"request":"$request",'
        '"status":$status,'
        '"body_bytes_sent":$body_bytes_sent,'
        '"http_referer":"$http_referer",'
        '"http_user_agent":"$http_user_agent",'
        '"http_x_forwarded_for":"$http_x_forwarded_for",'
        '"request_time":$request_time'
      '}';
    '';

    appendHttpConfig = ''
      access_log /var/log/nginx/access.log main_json;
    '';

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
    virtualHosts."vaultwarden.ftzmlab.xyz" = {
      enableACME = true;
      forceSSL = true;
      locations."/" = {
        proxyPass = "http://127.0.0.1:${toString config.services.vaultwarden.config.ROCKET_PORT}";
        proxyWebsockets = true;
      };
    };
    virtualHosts."deluge.ftzmlab.xyz" = {
      enableACME = true;
      forceSSL = true;
      locations."/" = {
        proxyPass = "http://127.0.0.1:${toString config.services.deluge.web.port}";
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
    virtualHosts."audiobookshelf.ftzmlab.xyz" = {
      enableACME = true;
      forceSSL = true;
      locations."/" = {
        proxyPass = "http://127.0.0.1:${toString config.services.audiobookshelf.port}";
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
        proxyPass = "http://127.0.0.1:2343";
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
    virtualHosts."muscleup.ftzmlab.xyz" = {
      enableACME = true;
      forceSSL = true;
      root = "/muscleup/";
      locations."/" = {
        extraConfig = ''
          add_header Cross-Origin-Embedder-Policy "credentialless";
          add_header Cross-Origin-Opener-Policy "same-origin";
        '';
      };
    };
  };

  fileSystems."/var/www/dav" = {
    device = "/mnt/nas/cloud";
    options = ["bind"];
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
}
