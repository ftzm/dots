# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

# ---------------------------------------------------------------------------
# Setup

{ self, system, nixpkgs, pkgs, config, lib, inputs, ... }:

# Define secrets in a separate file
let secrets = import ../secrets.nix;
in {
  imports = [
    inputs.home-manager.nixosModules.home-manager
    inputs.agenix.nixosModules.age
    ./sleep.nix
    ./users.nix
    ./cachix.nix
  ];

  nixpkgs = {
    config = {
      allowUnfree = true;
      permittedInsecurePackages = [ "p7zip-16.02" "openssl-1.0.2u" ];
    };
    overlays = [
      (import ../overlays)
      inputs.pipestatus.overlay
      inputs.emacs-overlay.overlay
      inputs.deploy-rs.overlay
    ];
  };

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

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    users.ftzm.imports = [ ../home ];
  };

  # ---------------------------------------------------------------------------
  # Boot

  boot = {
    # Use the systemd-boot EFI boot loader.
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
    initrd.luks.devices.root = {
      device = "/dev/nvme0n1p2";
      preLVM = true;
    };
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
    nfs-utils
    libnfs
    deploy-rs.deploy-rs
    inputs.agenix.defaultPackage.x86_64-linux
    alsaUtils
  ];

  environment.pathsToLink = [ "/share/zsh" ]; # for zsh completion

  virtualisation.docker.enable = true;

  programs.zsh.enable = true;

  hardware.bluetooth.enable = true;

  services.blueman.enable = true;

  services.sshd.enable = true;

  services.cron.enable = true;

  services.upower.enable = true;

  # for rpc-statd for nfs client: https://github.com/NixOS/nixpkgs/issues/76671
  services.nfs.server.enable = true;

  # ---------------------------------------------------------------------------
  # Security

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  networking.firewall.enable = false;

  # ---------------------------------------------------------------------------
  # Media

  # rtkit is optional but recommended
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;

    media-session.config.bluez-monitor.rules = [
      {
        # Matches all cards
        matches = [{ "device.name" = "~bluez_card.*"; }];
        actions = {
          "update-props" = {
            "bluez5.reconnect-profiles" = [ "hfp_hf" "hsp_hs" "a2dp_sink" ];
            # mSBC is not expected to work on all headset + adapter combinations.
            "bluez5.msbc-support" = true;
            # SBC-XQ is not expected to work on all headset + adapter combinations.
            "bluez5.sbc-xq-support" = true;
          };
        };
      }
      {
        matches = [
          # Matches all sources
          {
            "node.name" = "~bluez_input.*";
          }
          # Matches all outputs
          { "node.name" = "~bluez_output.*"; }
        ];
        actions = { "node.pause-on-idle" = false; };
      }
    ];
  };

  # Don't conflict with mopdiy
  # services.mpd = { enable = false; };

  programs.light.enable = true;

  services.mopidy = {
    enable = false;
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
      session = [{
        manage = "desktop";
        name = "home-manager";
        start = ''
          ${pkgs.runtimeShell} $HOME/.hm-session &
          waitPID=$!
        '';
      }];
      lightdm = { enable = true; };
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
    #enableFontDir = true;
    #fontDir.enable = true;
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
