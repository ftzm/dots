# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

  # ---------------------------------------------------------------------------
  # Setup

{ config, pkgs, ... }:

# Define secrets in a separate file
let
  secrets = import /etc/nixos/secrets.nix;
  sleepCheck = pkgs.writeScript "sleepCheck.sh" ''
  STATUS=$(cat /sys/class/power_supply/BAT0/status)
  CAPACITY=$(cat /sys/class/power_supply/BAT0/capacity)
  if [ "''${STATUS}" = "Discharging" ] && [ $CAPACITY -lt 5 ]; then
    systemctl hibernate
  fi
  '';
in

{
  imports =
    [ # Include the results of the hardware scan.
      <nixos-hardware/lenovo/thinkpad/t480s>
      /etc/nixos/hardware-configuration.nix
      /etc/nixos/state-version.nix
    ];

  nixpkgs.config.allowUnfree = true;


  # ---------------------------------------------------------------------------
  # Cache
  nix = {
    binaryCaches = [
      "https://cache.nixos.org"
      "https://cache.dhall-lang.org"
    ];

    binaryCachePublicKeys = [
      "cache.dhall-lang.org:I9/H18WHd60olG5GsIjolp7CtepSgJmM2CsO813VTmM="
    ];
  };

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
  time.timeZone = "Europe/Copenhagen";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    wget
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

  services.cron = {
    enable = true;
    systemCronJobs = [
    "*/5 * * * * root ${sleepCheck}"
    ];
  };

  services.upower.enable = true;

  hardware.brightnessctl.enable = true;

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
}
