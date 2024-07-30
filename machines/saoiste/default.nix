{
  inputs,
  lib,
  pkgs,
  ...
}: {
  imports = [
    # Nix extensions
    inputs.agenix.nixosModules.age
    inputs.home-manager.nixosModules.home-manager

    # Utils
    ../../role/home-setup.nix

    # System specific
    ./hardware.nix

    # Generic
    ../../role/network.nix
    ../../role/mpd.nix
    # ../../role/mail.nix
    ../../role/printing.nix
    ../../role/font.nix
    ../../role/audio.nix
    ../../role/git.nix
    ../../role/interface.nix
    ../../role/shell.nix
    ../../role/comms.nix
    ../../role/emacs.nix
    ../../role/wifi.nix
    ../../role/packages.nix
  ];

  users.extraUsers.ftzm = {
    createHome = true;
    extraGroups = ["wheel" "video" "audio" "disk" "networkmanager" "docker"];
    group = "users";
    home = "/home/ftzm";
    isNormalUser = true;
    uid = 1000;
  };

  # security.sudo.extraRules = [{
  #   users = [ "@wheel" ];
  #   commands = [{
  #     command = "/run/current-system/sw/bin/nix-store";
  #     options = [ "NOPASSWD" ];
  #   }];
  # }];

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

  nixpkgs = {
    config = {
      allowUnfree = true;
      permittedInsecurePackages = ["p7zip-16.02" "openssl-1.0.2u"];
    };
    overlays = [
      inputs.emacs-overlay.overlay
    ];
  };

  nix = {
    settings.trusted-users = ["@wheel"];
    package = pkgs.nixFlakes;
    extraOptions = ''
      experimental-features = nix-command flakes
      keep-outputs = true
      keep-derivations = true
      builders-use-substitutes = true
    '';
    buildMachines = [
      {
        hostName = "wg-nuc";
        sshUser = "admin";
        sshKey = "/home/ftzm/.ssh/id_rsa";
        system = "x86_64-linux";
        # if the builder supports building for multiple architectures,
        # replace the previous line by, e.g.,
        # systems = ["x86_64-linux" "aarch64-linux"];
        maxJobs = 4;
        speedFactor = 1;
        supportedFeatures = ["nixos-test" "benchmark" "big-parallel" "kvm"];
        mandatoryFeatures = [];
      }
    ];
    # distributedBuilds = true;
  };

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";

  # ssh setup
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.initrd.network.enable = true;
  boot.initrd.network.ssh = {
    enable = true;
    port = 22;
    shell = "/bin/cryptsetup-askpass";
    authorizedKeys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDjXUsGrBVN0jkm39AqfoEIG4PLxmefofNJPUtJeRnIoLZGMaS8Lw/tReVKx64+ttFWLAdkfi+djJHATxwMhhD8BwfJoP5RCz+3P97p1lQh6CjM0XrzTE9Ol6X1/D/mgS4oVa5YaVw3VszxN6Hm2BimKobvfHuIK5w/f0BoBIWxdvs0YyxCJvPsyIfmEvd8CPug9A8bo1/ni77AMpAWuw2RbEBJMk3sxHqUsHlCX/aPTjEqPusictHuy3xoHc4DSxgE/IZkV/d4wOzOUHaM+W8oKvBy8X00rMMprQ1e81WUySkh4UwgplNoD/hHGuVD0EN94ISkjwOfPGW0ACP7bVkZ"
    ];
    hostKeys = ["/etc/secrets/initrd/ssh_host_rsa_key"];
  };
  boot.kernelParams = ["ip=dhcp" "i915.force_probe=4c8a"];
  boot.initrd.availableKernelModules = ["r8169"];

  # Setup keyfile
  boot.initrd.secrets = {"/crypto_keyfile.bin" = null;};

  # Enable swap on luks
  boot.initrd.luks.devices."luks-80ee3586-78e6-4101-b35d-6c0bd7c3f26a".device = "/dev/disk/by-uuid/80ee3586-78e6-4101-b35d-6c0bd7c3f26a";
  boot.initrd.luks.devices."luks-80ee3586-78e6-4101-b35d-6c0bd7c3f26a".keyFile = "/crypto_keyfile.bin";

  networking.hostName = "saoiste"; # Define your hostname.
  networking.firewall.enable = false;
  networking.networkmanager.enable = true;

  system.stateVersion = "22.05";

  # ---------------------------------------------------------------------------
  # Home Manager

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
  };
  home-manager.users.ftzm.home.stateVersion = "21.05";
  home-manager.users.ftzm.home.activation = {
    myActivationAction = inputs.home-manager.lib.hm.dag.entryAfter ["writeBoundary"] ''
      $DRY_RUN_CMD /home/ftzm/.dots/dotfiles/install.sh -y \
        -f ${builtins.toPath ./../../dotfiles/MODULES}
    '';
  };

  # ---------------------------------------------------------------------------

  system.activationScripts = {
    # This is required to run third-party dynamically linked binaries
    # which expect their interpreter to be in the standard Linux FSH.
    ldso = lib.stringAfter ["usrbinenv"] ''
      mkdir -m 0755 -p /lib64
      ln -sfn ${pkgs.glibc.out}/lib64/ld-linux-x86-64.so.2 /lib64/ld-linux-x86-64.so.2.tmp
      mv -f /lib64/ld-linux-x86-64.so.2.tmp /lib64/ld-linux-x86-64.so.2 # atomically replace
    '';
  };

  time.timeZone = "Europe/Copenhagen";

  # nixpkgs.config.packageOverrides = pkgs: {
  #   vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
  # };
  hardware.opengl = {
    enable = true;
    driSupport = true;
    extraPackages = with pkgs; [
      intel-media-driver
      vaapiIntel
      vaapiVdpau
      libvdpau-va-gl
    ];
  };

  programs.steam.enable = true;

  services = {
    blueman.enable = true;
    sshd.enable = true;
    openssh.enable = true;
  };

  # services.openssh.permitRootLogin = "yes";

  services.syncthing = {
    enable = true;
    guiAddress = "0.0.0.0:8384";
    openDefaultPorts = true;
    user = "ftzm";
    configDir = "/home/ftzm/.config/syncthing";
    dataDir = "/home/ftzm";

    # overrides any devices added or deleted through the WebUI
    overrideDevices = true;
    # overrides any folders added or deleted through the WebUI
    overrideFolders = true;
  };

  services.xserver.videoDrivers = ["intel"];

  programs.tmux = {
    enable = true;
    # secureSocket = false;
    extraConfig = ''
      set -g xterm-keys on
      set -g default-terminal "xterm-256color"
      set -sg terminal-overrides ",*:RGB"
      set -g escape-time 0
      set -g status off

      unbind C-b
      set-option -g prefix C-a
      bind-key C-a send-prefix
    '';
  };

  # services.xserver.enable = true;
  # services.xserver.displayManager.gdm.enable = true;
  # services.xserver.desktopManager.gnome.enable = true;

  virtualisation.docker.enable = true;
  hardware.bluetooth.enable = true;

  users.users.ftzm.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDjXUsGrBVN0jkm39AqfoEIG4PLxmefofNJPUtJeRnIoLZGMaS8Lw/tReVKx64+ttFWLAdkfi+djJHATxwMhhD8BwfJoP5RCz+3P97p1lQh6CjM0XrzTE9Ol6X1/D/mgS4oVa5YaVw3VszxN6Hm2BimKobvfHuIK5w/f0BoBIWxdvs0YyxCJvPsyIfmEvd8CPug9A8bo1/ni77AMpAWuw2RbEBJMk3sxHqUsHlCX/aPTjEqPusictHuy3xoHc4DSxgE/IZkV/d4wOzOUHaM+W8oKvBy8X00rMMprQ1e81WUySkh4UwgplNoD/hHGuVD0EN94ISkjwOfPGW0ACP7bVkZ"
  ];

  # ----------------------------------------------------------------------
  # Atuin

  hm.programs.atuin = {
    enable = true;
    enableBashIntegration = true;
    settings = {
      auto_sync = true;
      sync_frequency = "5m";
      sync_address = "http://wg-nuc:8889";
      search_mode = "prefix";
    };
  };
}
