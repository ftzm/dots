  # ---------------------------------------------------------------------------
  # System

  virtualisation.docker.enable = true;
  hardware.bluetooth.enable = true;
  services = {
    blueman.enable = true;
    sshd.enable = true;
    cron.enable = true;
  #   upower.enable = true;
  #   # for rpc-statd for nfs client: https://github.com/NixOS/nixpkgs/issues/76671
  #   nfs.server.enable = true;
  };

  # programs.mosh.enable = true;

  # ---------------------------------------------------------------------------
  # Media

  # rtkit is optional but recommended
  # security.rtkit.enable = true;

  # hardware.pulseaudio.enable = false;
  # services.pipewire = {
  #   enable = true;
  # };
  # services = {
  #   alsa.enable = true;
  #   jack.enable = true;
  #   pulse.enable = true;
  # };

  # ---------------------------------------------------------------------------
  # GUI

  #qt5.platformTheme = "qt5ct";

}
