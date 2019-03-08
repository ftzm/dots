# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

  # ---------------------------------------------------------------------------
  # Setup

{ config, pkgs, ... }:

# Define secrets in a separate file
let
  secrets = import /etc/nixos/secrets.nix;
in

{
  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
    ];

  nixpkgs.config.allowUnfree = true;

  # ---------------------------------------------------------------------------
  # Boot

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.luks.devices = [
    {
      name = "root";
      device = "/dev/nvme0n1p2";
      preLVM = true;
    }
  ];

  # ---------------------------------------------------------------------------
  # System

  networking.hostName = "oibri-nixos"; # Define your hostname.
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "US/Eastern";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    wget
    vim
    git
    gnupg
    hsetroot # for desktopManager
  ];

  virtualisation.docker.enable = true;

  programs.zsh.enable = true;

  hardware.bluetooth.enable = true;

  # ---------------------------------------------------------------------------
  # Security

  # Enable gnupg agent
  programs.gnupg.agent.enable = true;
  programs.gnupg.agent.enableSSHSupport = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  networking.firewall.enable = false;

  # ---------------------------------------------------------------------------
  # Media

  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull; # For bluetooth
    support32Bit = true;
    tcp = {
      # for mopidy
      enable = true;
      anonymousClients.allowedIpRanges = ["127.0.0.1"];
    };
  };

  # Don't conflict with mopdiy
  services.mpd = {
    enable = false;
  };

  services.mopidy = {
    enable = true;
    extensionPackages = [ pkgs.mopidy-spotify ];
    configuration = ''
      [spotify]
      username = ${secrets.spotify.username}
      password = ${secrets.spotify.password}
      client_id = ${secrets.spotify.client_id}
      client_secret = ${secrets.spotify.client_secret}

      [audio]
      output = pulsesink server=127.0.0.1
    '';
  };

  # ---------------------------------------------------------------------------
  # X

  # Needed for steam
  hardware.opengl.driSupport32Bit = true;

  hardware.nvidia.optimus_prime.enable = true;
  # Bus ID of the NVIDIA GPU. You can find it using lspci
  hardware.nvidia.optimus_prime.nvidiaBusId = "PCI:02:00:0";
  # Bus ID of the Intel GPU. You can find it using lspci
  hardware.nvidia.optimus_prime.intelBusId = "PCI:00:02:0";

  services.xserver = {
    enable = true;
    layout = "us";

    videoDrivers = [ "nvidiaBeta" ];

    displayManager.lightdm = {
      enable = true;
      autoLogin.enable = true;
      autoLogin.user = "matt";
    };

    windowManager = {
      xmonad.enable = true;
      xmonad.enableContribAndExtras = true;
      default = "xmonad";
    };

    desktopManager = {
      session = [
        {
          name = "custom";
          start = ''
            ${pkgs.hsetroot}/bin/hsetroot -solid "#282828" &
            xsetroot -cursor_name left_ptr &
            xrdb -merge ~/.Xresources
            ${pkgs.dunst}/bin/dunst &
          '';
        }
      ];
      xterm.enable = false;
      default = "custom";
    };
  };

  # Enable slock for screen locking
  programs.slock.enable = true;

  # Enable touchpad support.
  services.xserver.libinput.enable = true;

  # Fonts
  fonts = {
    fontconfig.enable = true;
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      fira-mono
      fira-code
      terminus_font
      inconsolata
      ttf_bitstream_vera
      font-awesome-ttf
      iosevka
      proggyfonts
      liberation_ttf
    ];
  };

  # ---------------------------------------------------------------------------
  # Users

  # Define a user account. Don't forget to set a password with ‘passwd’.
  # users.users.guest = {
  #   isNormalUser = true;
  #   uid = 1000;
  # };
  users.extraUsers.matt = {
    createHome = true;
    extraGroups = [
      "wheel"
      "video"
      "audio"
      "disk"
      "networkmanager"
      "docker"
    ];
    group = "users";
    home = "/home/matt";
    isNormalUser = true;
    shell = pkgs.zsh;
    uid = 1000;
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.09"; # Did you read the comment?

}
