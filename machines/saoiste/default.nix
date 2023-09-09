{ inputs, lib, pkgs, ... }:

{
  imports = [
    ./hardware.nix
    ../../configuration/network.nix
  ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";

  # Setup keyfile
  boot.initrd.secrets = {
    "/crypto_keyfile.bin" = null;
  };

  # Enable swap on luks
  boot.initrd.luks.devices."luks-80ee3586-78e6-4101-b35d-6c0bd7c3f26a".device = "/dev/disk/by-uuid/80ee3586-78e6-4101-b35d-6c0bd7c3f26a";
  boot.initrd.luks.devices."luks-80ee3586-78e6-4101-b35d-6c0bd7c3f26a".keyFile = "/crypto_keyfile.bin";

  networking.hostName = "saoiste"; # Define your hostname.
  networking.networkmanager.enable = true;

  system.stateVersion = "22.05";
  #nix.settings.maxJobs = lib.mkDefault 8;
  home-manager.users.ftzm.imports = [ ./home.nix  ];


  # make things work
  time.timeZone = "Europe/Copenhagen";

  # nixpkgs.config.packageOverrides = pkgs: {
  #   vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
  # };
  # hardware.opengl = {
  #   enable = true;
  #   driSupport = true;
  #   extraPackages = with pkgs; [
  #     intel-media-driver
  #     vaapiIntel
  #     vaapiVdpau
  #     libvdpau-va-gl
  #   ];
  # };

  # services = {
  #   sshd.enable = true;
  # };

  # services.openssh.permitRootLogin = "yes";

  # services.syncthing = {
  #   enable = true;
  #   #guiAddress = "localhost:8384";
  #   openDefaultPorts = true;
  #   user = "ftzm";
  #   configDir = "/home/ftzm/.config/syncthing";
  #   dataDir = "/home/ftzm";

  #   # overrides any devices added or deleted through the WebUI
  #   overrideDevices = true;
  #   # overrides any folders added or deleted through the WebUI
  #   overrideFolders = true;


  # };


  services.xserver.videoDrivers = [ "intel" ];
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.kernelParams = [ "i915.force_probe=4c8a" ];

  programs.tmux = {
    enable = true;
    # secureSocket = false;
    extraConfig = ''
      set -g xterm-keys on
      set -g default-terminal "xterm-256color"
      set -sg terminal-overrides ",*:RGB"
      set -g escape-time 0
      set -g status off
    '';
  };

}
