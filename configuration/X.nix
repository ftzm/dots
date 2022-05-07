{ pkgs, ... }:
{
  #X
  services.xserver = {
    enable = true;
    layout = "us";

    # Enable touchpad support.
    libinput.enable = true;
  };
  services.xserver.autorun = false;
  services.xserver.displayManager.startx.enable = true;

  # Needed for steam
  hardware.opengl.driSupport32Bit = true;

}
