{ self, system, nixpkgs, pkgs, config, lib, inputs, ... }:
{
  imports = [
    # System specific
    ./hardware.nix
    # Generic
    inputs.home-manager.nixosModules.home-manager
    # ../../role/home-conf.nix # Allows using home-conf attribute
    inputs.agenix.nixosModules.age
    inputs.nixos-hardware.nixosModules.lenovo-thinkpad-x1-7th-gen
    ../../configuration/sleep.nix
    ../../configuration/users.nix
    ../../configuration/cachix.nix
    ../../configuration/network.nix
    ../../role/mpd.nix
    ../../role/mail.nix
    ../../role/printing.nix
    ../../role/font.nix
    ../../role/audio.nix
    ../../role/git.nix
    ../../role/pipestatus.nix
    ../../role/wayland.nix
    ../../role/shell.nix
    ../../role/comms.nix
    ../../role/emacs.nix
    ../../role/wifi.nix
    ../../role/packages.nix
    ../../role/notification.nix
  ];

  # home-conf.home.packages = with pkgs; [ awscli lsb-release discord ];
  personal.font_size = 10.0;
  personal.zsh_extra = "";

  nixpkgs = {
    config = {
      allowUnfree = true;
      permittedInsecurePackages = [ "p7zip-16.02" "openssl-1.0.2u" ];
      allowBroken = true;
    };
    overlays = [
      (import ../../overlays)
      inputs.pipestatus.overlay
      inputs.emacs-overlay.overlay
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
  };
  home-manager.users.ftzm.home.stateVersion = "21.05";
  home-manager.users.ftzm.home.activation = {
    myActivationAction = inputs.home-manager.lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      $DRY_RUN_CMD /home/ftzm/.dots/dotfiles/install.sh -y \
        -f ${builtins.toPath ./../../dotfiles/MODULES}
    '';
  };

  # System

  # ---------------------------------------------------------------------------
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
    inputs.agenix.packages.x86_64-linux.agenix
  ];

  virtualisation.docker.enable = true;
  hardware.bluetooth.enable = true;
  services = {
    blueman.enable = true;
    sshd.enable = true;
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


  # ---------------------------------------------------------------------------
  # GUI

  qt.platformTheme = "qt5ct";

  # Allows building for arch64
  # Current use-case is building system images for raspberrypi
  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];

  networking = {
    hostName = "leigheas"; # Define your hostname.
    useDHCP = false;
    interfaces = {
      enp0s31f6.useDHCP = true;
      wlp0s20f3.useDHCP = true;
    };
  };
  # networking.networkmanager.insertNameservers = ["10.2.34.189"];
  # networking.resolvconf.extraConfig = ''
  #   name_servers="10.2.35.177 10.2.34.189"
  # '';
  # networking.resolvconf.enable = false;
  networking.nameservers = [ "1.1.1.1" ];



  services.syncthing = {
    enable = true;
    #guiAddress = "localhost:8384";
    openDefaultPorts = true;
    user = "ftzm";
    configDir = "/home/ftzm/.config/syncthing";
    dataDir = "/home/ftzm";
  };

  # System specific DPI
  services.xserver.dpi = 192;

  programs.steam = { enable = true; };

  system.stateVersion = "20.09";

}
