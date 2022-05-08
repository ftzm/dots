# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

# ---------------------------------------------------------------------------
# Setup

{ self, system, nixpkgs, pkgs, config, lib, inputs, ... }:
let
  iosevkaPkgs = inputs.nixpkgs-iosevka.legacyPackages.x86_64-linux;
  iosevkaLig = pkgs.callPackage ../iosevka { pkgs = iosevkaPkgs; };
in {
  imports = [
    inputs.home-manager.nixosModules.home-manager
    inputs.agenix.nixosModules.age
    ./sleep.nix
    ./users.nix
    ./cachix.nix
    ./wayland.nix
    ./X.nix
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
      builders-use-substitutes = true
    '';
    buildMachines = [{
      hostName = "wg-nuc";
      sshUser = "admin";
      sshKey = "/home/ftzm/.ssh/id_rsa";
      system = "x86_64-linux";
      # if the builder supports building for multiple architectures,
      # replace the previous line by, e.g.,
      # systems = ["x86_64-linux" "aarch64-linux"];
      maxJobs = 4;
      speedFactor = 1;
      supportedFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
      mandatoryFeatures = [ ];
    }];
    distributedBuilds = true;
  };

  # ---------------------------------------------------------------------------
  # Home Manager

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    users.ftzm.imports = [ ../home ];
  };

  age.secrets = {
    fitzmattd-email = {
      file = ../secrets/fitzmattd-email.age;
      owner = "ftzm";
    };
    ftzm-org-email = {
      file = ../secrets/ftzm-org-email.age;
      owner = "ftzm";
    };
  };
  home-manager.users.ftzm = {
    accounts.email.accounts.fitzmattd.passwordCommand =
      "${pkgs.coreutils}/bin/cat ${config.age.secrets.fitzmattd-email.path}";
    accounts.email.accounts.ftzm.passwordCommand =
      "${pkgs.coreutils}/bin/cat ${config.age.secrets.ftzm-org-email.path}";
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

  # Set your time zone.
  time.timeZone = "Europe/Copenhagen";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    wget
    curl
    vim
    htop
    git
    nfs-utils
    libnfs
    deploy-rs.deploy-rs
    inputs.agenix.defaultPackage.x86_64-linux
    alsaUtils
    # X
    alacritty
    # for screen sharing
    slurp
  ];
  environment.pathsToLink = [ "/share/zsh" ]; # for zsh completion
  programs.zsh.enable = true;

  # services.earlyoom.enable = true;

  virtualisation.docker.enable = true;
  hardware.bluetooth.enable = true;
  services = {
    blueman.enable = true;
    sshd.enable = true;
    cron.enable = true;
    upower.enable = true;
    # for rpc-statd for nfs client: https://github.com/NixOS/nixpkgs/issues/76671
    nfs.server.enable = true;
  };

  programs.mosh.enable = true;

  # ---------------------------------------------------------------------------
  # Networking

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  networking.firewall.enable = false;
  networking.networkmanager.enable = true;

  # Cheeky hack to restart the wireguard service on wifi connection.
  # Easiest way to re-resolve hostnames on new network.
  networking.networkmanager.dispatcherScripts = [{
    source = pkgs.writeText "upHook" ''
      if [ $1 != "wg0" ]; then
          case "$2" in
              #up|vpn-up)
              up)
                logger -s " interface $1 up, restarting wireguard"
                ${pkgs.systemd}/bin/systemctl restart wireguard-wg0.service
              ;;
              down|vpn-down)
              ;;
              hostname|dhcp4-change|dhcp6-change)
              # Do nothing
              ;;
              *)
              echo "$0: called with unknown action \`$2'" 1>&2
              exit 1
              ;;
          esac
      fi
    '';
    type = "basic";
  }];

  # ---------------------------------------------------------------------------
  # Media

  # rtkit is optional but recommended
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # wireplumber.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    media-session.enable = false;
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
  # ---------------------------------------------------------------------------
  # GUI

  programs.qt5ct.enable = true;

  fonts = {
    fontconfig.enable = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [ font-awesome iosevkaLig ];
  };

}
