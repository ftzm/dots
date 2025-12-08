{pkgs, ...}: {
  # rtkit is optional but recommended
  security.rtkit.enable = true;

  environment.systemPackages = with pkgs; [
    alsa-utils
  ];

  services.pulseaudio.enable = false;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    jack.enable = true;
    pulse.enable = true;
  };
}
