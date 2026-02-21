# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  config,
  pkgs,
  inputs,
  lib,
  modulesPath,
  ...
}: {
  imports = [
    # Include the results of the hardware scan.
    inputs.agenix.nixosModules.age
    ./hardware.nix
    (modulesPath + "/installer/sd-card/sd-image-aarch64.nix")
    inputs.nixos-hardware.nixosModules.raspberry-pi."3"
  ];

  # Ensure firmware partition is large enough for kernel + initrd
  sdImage.firmwareSize = 256;

  # Disable ZFS (broken with latest kernel)
  boot.supportedFilesystems.zfs = lib.mkForce false;

  # make members of wheel group trusted users, allowing them additional rights when
  # connection to nix daemon.
  # This was enable to allow deploying via deploy-rs as non-root.
  nix.settings.trustedUsers = ["@wheel"];

  # Use the extlinux boot loader. (NixOS wants to enable GRUB by default)
  boot.loader.grub.enable = false;
  # Enables the generation of /boot/extlinux/extlinux.conf
  boot.loader.generic-extlinux-compatible.enable = true;

  boot.kernelPackages = pkgs.linuxPackages_latest;

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
    extraGroups = ["wheel"]; # Enable ‘sudo’ for the user.
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDjXUsGrBVN0jkm39AqfoEIG4PLxmefofNJPUtJeRnIoLZGMaS8Lw/tReVKx64+ttFWLAdkfi+djJHATxwMhhD8BwfJoP5RCz+3P97p1lQh6CjM0XrzTE9Ol6X1/D/mgS4oVa5YaVw3VszxN6Hm2BimKobvfHuIK5w/f0BoBIWxdvs0YyxCJvPsyIfmEvd8CPug9A8bo1/ni77AMpAWuw2RbEBJMk3sxHqUsHlCX/aPTjEqPusictHuy3xoHc4DSxgE/IZkV/d4wOzOUHaM+W8oKvBy8X00rMMprQ1e81WUySkh4UwgplNoD/hHGuVD0EN94ISkjwOfPGW0ACP7bVkZ"
    ];
  };

  # users.users.root.openssh.authorizedKeys.keys = [
  #   "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDjXUsGrBVN0jkm39AqfoEIG4PLxmefofNJPUtJeRnIoLZGMaS8Lw/tReVKx64+ttFWLAdkfi+djJHATxwMhhD8BwfJoP5RC z+3P97p1lQh6CjM0XrzTE9Ol6X1/D/mgS4oVa5YaVw3VszxN6Hm2BimKobvfHuIK5w/f0BoBIWxdvs0YyxCJvPsyIfmEvd8CPug9A8bo1/ni77AMpAWuw2RbEBJMk3sxHqUsHlCX/a PTjEqPusictHuy3xoHc4DSxgE/IZkV/d4wOzOUHaM+W8oKvBy8X00rMMprQ1e81WUySkh4UwgplNoD/hHGuVD0EN94ISkjwOfPGW0ACP7bVkZ"
  # ];

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
  ];

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  # services.openssh.permitRootLogin = "yes";

  virtualisation.oci-containers.containers = {
    pihole = {
      image = "pihole/pihole:latest";
      ports = ["53:53/tcp" "53:53/udp" "80:80" "443:443"];
      volumes = [
        "/var/lib/pihole/:/etc/pihole/"
        "/var/lib/dnsmasq.d:/etc/dnsmasq.d/"
      ];
      environment = {
        TZ = "Europe/Copenhagen";
        WEBPASSWORD = "admin";
      };
      extraOptions = ["--cap-add=NET_ADMIN" "--dns=127.0.0.1" "--dns=1.1.1.1"];
      workdir = "/var/lib/pihole/";
    };
  };

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
        nameservers.global = [
          "1.1.1.1"
          "1.0.0.1"
        ];
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
  networking.firewall.allowedTCPPorts = [8443];

  system.stateVersion = "21.05"; # Did you read the comment?
}
