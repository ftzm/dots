# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

# ---------------------------------------------------------------------------
# Setup

{ config, lib, ... }:

# Define secrets in a separate file
let
  sources = import ../nix/sources.nix;
  nixos-hardware = sources.nixos-hardware.outPath;
  nixpkgs = import sources.nixpkgs {
    config = {
      allowUnfree = true;
      checkMeta = true;
    };
  };
  pkgs = nixpkgs.pkgs;
  secrets = import ../secrets.nix;

in {
  imports = [ # Include the results of the hardware scan.
    "${nixos-hardware}/lenovo/thinkpad/t480s"
    /etc/nixos/hardware-configuration.nix
    /etc/nixos/state-version.nix
    /etc/nixos/cachix.nix
    ./caches.nix
    ./sleep.nix
    ./users.nix
  ];

  nix.nixPath = lib.mkForce [
    "nixpkgs=${sources.nixpkgs.outPath}"
    "nixos-config=/etc/nixos/configuration.nix"
    #"config-checkout=${config.configCheckout}"
  ];

  # ---------------------------------------------------------------------------
  # Boot

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.luks.devices = [{
    name = "root";
    device = "/dev/nvme0n1p2";
    preLVM = true;
  }];

  # ---------------------------------------------------------------------------
  # System

  networking.hostName = "oibri-nixos"; # Define your hostname.
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "Europe/Copenhagen";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    wget
    curl
    vim
    git
    gnupg
    hsetroot # for desktopManager
    steam
  ];

  environment.pathsToLink = [ "/share/zsh" ]; # for zsh completion

  virtualisation.docker.enable = true;

  programs.zsh.enable = true;

  hardware.bluetooth.enable = true;

  services.cron.enable = true;

  services.upower.enable = true;

  hardware.brightnessctl.enable = true;

  # ---------------------------------------------------------------------------
  # Security

  # Enable gnupg agent
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

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
      anonymousClients.allowedIpRanges = [ "127.0.0.1" ];
    };
  };

  # Don't conflict with mopdiy
  services.mpd = { enable = false; };

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

  services.xserver = {
    enable = true;
    layout = "us";

    displayManager.lightdm = {
      enable = true;
      autoLogin.enable = true;
      autoLogin.user = "matt";
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
      terminus_font
      inconsolata
      ttf_bitstream_vera
      font-awesome-ttf
      proggyfonts
      liberation_ttf
    ];
  };
}