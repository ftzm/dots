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

  # Use the GRUB 2 boot loader.
  # boot.loader.grub.enable = true;
  # boot.loader.grub.version = 2;
  # boot.loader.grub.efiSupport = true;
  # boot.loader.grub.efiInstallAsRemovable = true;
  # boot.loader.efi.efiSysMountPoint = "/boot/efi";
  # Define on which hard drive you want to install Grub.
  # boot.loader.grub.device = "/dev/sda"; # or "nodev" for efi only
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

  networking.hostName = "nas"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Set your time zone.
  time.timeZone = "Europe/Copenhagen";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.eno1.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };

  # Configure keymap in X11
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  # sound.enable = true;
  # hardware.pulseaudio.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

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
    lshw
    smartmontools
    hdparm
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

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

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
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

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?

}
