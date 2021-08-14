{ config, pkgs, inputs, ... }:

{
  imports = [ # Include the results of the hardware scan.
    inputs.agenix.nixosModules.age
    ./hardware.nix
  ];

  # make members of wheel group trusted users, allowing them additional rights when
  # connection to nix daemon.
  # This was enable to allow deploying via deploy-rs as non-root.
  nix.trustedUsers = [ "@wheel" ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "nuc"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Set your time zone.
  time.timeZone = "Europe/Copenhagen";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.eno1.useDHCP = true;
  networking.interfaces.wlp0s20f3.useDHCP = true;

  age.secrets.wireguard-private-key-nuc.file =
    ../../secrets/wireguard-private-key-nuc.age;

  networking = {
    wireguard.interfaces = {
      wg0 = {
        ips = [ "10.0.100.4/24" ];
        listenPort = 51840;
        privateKeyFile = "/home/admin/.wg/nuc.key";
        peers = [{
          publicKey = "eLpLj1/WiCEW8w00A+HLPMkUGTGRCrRb1znESP43q0I=";
          allowedIPs = [ "10.0.100.2" ];
        }];
      };
    };
  };

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  # Configure keymap in X11
  services.xserver.layout = "us";

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.admin = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    wget
    vim
    mpc_cli
    ncmpcpp
    (beets.override { enableExtraFiles = true; })
    sqlite
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;
  services = { openssh.enable = true; };

  services.sshd.enable = true;

  networking.firewall.enable = false;

  services.jellyfin = {
    enable = true;
    group = "users";
  };

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

  # nginx reverse proxy
  services.nginx = {
    enable = true;
    virtualHosts.${config.services.grafana.domain} = {
      locations."/" = {
        proxyPass = "http://127.0.0.1:${toString config.services.grafana.port}";
        proxyWebsockets = true;
      };
    };
  };

  # Hack to fix deploy-rs password entry
  # https://github.com/serokell/deploy-rs/issues/78
  environment.etc."sudo.conf" = {
    mode = "0400";
    # uncomment one of the following lines
    #text = "Path askpass ${askpass}";
    #text = "Path askpass ${pkgs.x11_ssh_askpass}/libexec/x11-ssh-askpass";
    #text = "Path askpass ${pkgs.ssh-askpass-fullscreen}/bin/ssh-askpass-fullscreen";
    text =
      "Path askpass ${pkgs.lxqt.lxqt-openssh-askpass}/bin/lxqt-openssh-askpass";
    #text = "Path askpass ${pkgs.ksshaskpass}/bin/ksshaskpass";
  };
  services.openssh.forwardX11 = true; # The server must allow X11 forwarding

  system.stateVersion = "20.09";
}
