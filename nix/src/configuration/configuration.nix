# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

# ---------------------------------------------------------------------------
# Setup

{ self, system, nixpkgs, pkgs, config, lib, ... }:

# Define secrets in a separate file
let
  secrets = import ../secrets.nix;
in {
  imports = [
    ./sleep.nix
    ./users.nix
  ];

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.permittedInsecurePackages = [
    "p7zip-16.02"
  ];
  nix = {
    package = pkgs.nixFlakes;
    extraOptions = ''
      experimental-features = nix-command flakes
      keep-outputs = true
      keep-derivations = true
    '';
   };

  # ---------------------------------------------------------------------------
  # Home Manager

  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = true;

  # ---------------------------------------------------------------------------
  # Boot

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.luks.devices.root = {
    device = "/dev/nvme0n1p2";
    preLVM = true;
  };

  # ---------------------------------------------------------------------------
  # System

  system.activationScripts = {
    # This is required to run third-party dynamically linked binaries
    # which expect their interpreter to be in the standard Linux FSH.
    ldso = lib.stringAfter [ "usrbinenv" ] ''
       mkdir -m 0755 -p /lib64
       ln -sfn ${pkgs.glibc.out}/lib64/ld-linux-x86-64.so.2 /lib64/ld-linux-x86-64.so.2.tmp
       mv -f /lib64/ld-linux-x86-64.so.2.tmp /lib64/ld-linux-x86-64.so.2 # atomically replace
     '';
  };

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
    hsetroot # for desktopManager
    brightnessctl
  ];

  environment.pathsToLink = [ "/share/zsh" ]; # for zsh completion

  virtualisation.docker.enable = true;

  programs.zsh.enable = true;

  hardware.bluetooth.enable = true;

  services.cron.enable = true;

  services.upower.enable = true;



  # ---------------------------------------------------------------------------
  # Security

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

  programs.light.enable = true;

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

    displayManager = {
      session = [
        { manage = "desktop";
          name = "home-manager";
          start = ''
            ${pkgs.runtimeShell} $HOME/.hm-session &
            waitPID=$!
          '';
        }
      ];
      lightdm = {
        enable = true;
      };
      autoLogin.enable = true;
      autoLogin.user = "ftzm";
      defaultSession = "home-manager";
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
