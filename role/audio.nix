{ pkgs, ... }:

{
  # rtkit is optional but recommended
  security.rtkit.enable = true;

  environment.systemPackages = with pkgs; [
    alsaUtils
  ];

  hardware.pulseaudio.enable = false;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    jack.enable = true;
    pulse.enable = true;
  };
}
