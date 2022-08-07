{ inputs, lib, ... }:

{
  imports = [
    ./hardware.nix
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
  system.stateVersion = "22.05";
  #nix.settings.maxJobs = lib.mkDefault 8;
  home-manager.users.ftzm.imports = [ ./home.nix  ];
}
