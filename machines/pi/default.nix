# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running 'nixos-help').
{inputs}:
# nixosSystem, not nixosInstaller: the installer variant is for building
# installer media -- it adds NixOS's installation-device profile (console
# autologin, empty root password, PermitRootLogin yes) and the RPi-optimised
# overlays globally, whose ffmpeg rebuilt a python test chain under emulation
# (headscale.yaml -> remarshal -> ... -> matplotlib -> ffmpeg dev). The
# optimised packages stay available as pkgs.rpi.
inputs.nixos-raspberrypi.lib.nixosSystem {
  specialArgs = inputs;
  modules = [
    {
      # Hardware specific configuration, see section below for a more complete
      # list of modules
      imports = with inputs.nixos-raspberrypi.nixosModules; [
        raspberry-pi-3.base
      ];
    }

    # The system lives on the SD card it was installed from. Its layout,
    # declared directly as upstream's demo does for an SD-resident Pi
    # (nvmd/nixos-raspberrypi-demo, rpi02) instead of importing the sd-image
    # module, which only an image build needs.
    {
      fileSystems = {
        "/boot/firmware" = {
          device = "/dev/disk/by-label/FIRMWARE";
          fsType = "vfat";
          options = [
            "noatime"
            "noauto"
            "x-systemd.automount"
            "x-systemd.idle-timeout=1min"
          ];
        };
        "/" = {
          device = "/dev/disk/by-label/NIXOS_SD";
          fsType = "ext4";
          options = ["noatime"];
        };
      };
    }

    ({
      config,
      pkgs,
      lib,
      ...
    }: {
      system.nixos.tags = let
        cfg = config.boot.loader.raspberry-pi;
      in [
        "raspberry-pi-${cfg.variant}"
        cfg.bootloader
        config.boot.kernelPackages.kernel.version
      ];

      # make members of wheel group trusted users, allowing them additional rights when
      # connection to nix daemon.
      # This was enable to allow deploying via deploy-rs as non-root.
      nix.settings.trusted-users = ["@wheel"];

      networking.hostName = "pi"; # Define your hostname.

      # Set your time zone.
      time.timeZone = "Europe/Copenhagen";

      # The global useDHCP flag is deprecated, therefore explicitly set to false here.
      # Per-interface useDHCP will be mandatory in the future, so this generated config
      # replicates the default behaviour.
      networking.useDHCP = false;
      networking.interfaces.eth0.useDHCP = true;

      # Configure network proxy if necessary
      # networking.proxy.default = "http://user:password@proxy:port/";
      # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

      users.users.admin = {
        isNormalUser = true;
        initialPassword = "changeme";
        extraGroups = ["wheel"]; # Enable 'sudo' for the user.
        openssh.authorizedKeys.keys = [
          "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDjXUsGrBVN0jkm39AqfoEIG4PLxmefofNJPUtJeRnIoLZGMaS8Lw/tReVKx64+ttFWLAdkfi+djJHATxwMhhD8BwfJoP5RCz+3P97p1lQh6CjM0XrzTE9Ol6X1/D/mgS4oVa5YaVw3VszxN6Hm2BimKobvfHuIK5w/f0BoBIWxdvs0YyxCJvPsyIfmEvd8CPug9A8bo1/ni77AMpAWuw2RbEBJMk3sxHqUsHlCX/aPTjEqPusictHuy3xoHc4DSxgE/IZkV/d4wOzOUHaM+W8oKvBy8X00rMMprQ1e81WUySkh4UwgplNoD/hHGuVD0EN94ISkjwOfPGW0ACP7bVkZ"
        ];
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

      # List packages installed in system profile. To search, run:
      # $ nix search wget
      environment.systemPackages = with pkgs; [
        vim
        foot.terminfo
        wget
      ];

      # Enable the OpenSSH daemon.
      services.openssh.enable = true;
      # services.openssh.permitRootLogin = "yes";

      # ---------------------------------------------------------------------------
      # ddclient

      age.secrets.ddclient = {
        file = ../../secrets/ddclient.age;
      };
      services.ddclient = {
        enable = true;
        configFile = config.age.secrets.ddclient.path;
      };
      # The default method of installing the configFile wasn't working at time of writing
      systemd.services.ddclient.serviceConfig.LoadCredential = "config:${config.age.secrets.ddclient.path}";
      systemd.services.ddclient.serviceConfig.ExecStartPre = lib.mkForce ''${lib.getBin pkgs.bash}/bin/bash -c "${lib.getBin pkgs.coreutils}/bin/ln -s $CREDENTIALS_DIRECTORY/config /run/ddclient/ddclient.conf"'';

      # ----------------------------------------------------------------------
      # Headscale - self-hosted Tailscale control server

      services.headscale = {
        enable = true;
        address = "127.0.0.1";
        port = 8080;
        settings = {
          server_url = "https://headscale.ftzmlab.xyz:8443";
          dns = {
            base_domain = "tail.ftzmlab.xyz";
            magic_dns = true;
            nameservers = {
              global = [
                "100.64.0.2"
                "1.1.1.1"
                "1.0.0.1"
              ];
              split = {
                "localdomain" = ["192.168.1.1"];
                "lan.ftzmlab.xyz" = ["100.64.0.2"];
              };
            };
          };
          logtail.enabled = false;
        };
      };

      # Cloudflare API credentials for ACME DNS-01 challenge
      age.secrets.cloudflare-api = {
        file = ../../secrets/cloudflare-api.age;
      };

      # ACME with DNS-01 challenge via Cloudflare
      security.acme = {
        acceptTerms = true;
        defaults.email = "fitz.matt.d@gmail.com";
        certs."headscale.ftzmlab.xyz" = {
          dnsProvider = "cloudflare";
          environmentFile = config.age.secrets.cloudflare-api.path;
          group = "nginx";
        };
      };

      # Nginx reverse proxy with SSL on port 8443
      services.nginx = {
        enable = true;
        recommendedProxySettings = true;
        recommendedTlsSettings = true;
        virtualHosts."headscale.ftzmlab.xyz" = {
          listen = [
            {
              addr = "0.0.0.0";
              port = 8443;
              ssl = true;
            }
          ];
          useACMEHost = "headscale.ftzmlab.xyz";
          forceSSL = true;
          locations."/" = {
            proxyPass = "http://127.0.0.1:8080";
            proxyWebsockets = true;
          };
        };
      };

      # Open ports in the firewall.
      # 8443 headscale; 9002 node_exporter, scraped by the cluster over the LAN.
      networking.firewall.allowedTCPPorts = [8443 9002];

      system.stateVersion = "21.05"; # Did you read the comment?
    })

    inputs.agenix.nixosModules.age
    ../../role/lab.nix
    ../../role/fleet-host.nix
    ./headscale-state.nix
    {
      # Deployed by fleet-agent: the pi cannot build its own system (it never
      # ran comin). 29 GB SD card with a 5 GiB closure: keep fewer generations.
      fleetHost = {
        transport = "lan";
        keepGenerations = 3;
        autoRebootAt = "05:00";
      };
    }
    ({config, ...}: {
      age.secrets.headscale-noise-key = {
        file = ../../secrets/headscale-noise-key.age;
        owner = config.services.headscale.user;
        group = config.services.headscale.group;
        mode = "0400";
      };
      headscaleState = {
        noiseKeyFile = config.age.secrets.headscale-noise-key.path;
        # The key headscale generated on this pi, carried into agenix.
        noiseKeySha256 = "a26cf6c799470591d033b803d29d86c3562c1b3059cb1f40086b2d5116ac0c61";
      };
    })
  ];
}
