{ pkgs, lib, ... }:
{
  #needed for sway to work
  hardware.opengl.enable = lib.mkDefault true;
  fonts.enableDefaultFonts = lib.mkDefault true;
  programs.dconf.enable = lib.mkDefault true;

  # wayland screen sharing
  services.dbus.enable = true;
  xdg = {
    portal = {
      enable = true;
      extraPortals = with pkgs; [
        xdg-desktop-portal-wlr
        xdg-desktop-portal-gtk
      ];
      # Deprecated. People who really want it and aware of the downsides can just set
      # `environment.sessionVariables.GTK_USE_PORTAL = "1";` NixOS option
      # directly to set the environment variable globally.
      # gtkUsePortal = true;
    };
  };

  #needed for swaylock to work
  security.pam.services.swaylock = { };

  # command to adjust screen brightness
  programs.light.enable = true;

  # Sway
  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
    # https://man.sr.ht/~kennylevinsen/greetd/#how-to-set-xdg_session_typewayland
    #TODO: put this in a reusable script
    extraSessionCommands = ''
      #!/bin/sh
      # Session
      export XDG_SESSION_TYPE=wayland
      export XDG_SESSION_DESKTOP=sway
      export XDG_CURRENT_DESKTOP=sway
      export MOZ_ENABLE_WAYLAND=1
      export NIXOS_OZONE_WL=1
      export CLUTTER_BACKEND=wayland
      export QT_QPA_PLATFORM=wayland-egl
      export ECORE_EVAS_ENGINE=wayland-egl
      export ELM_ENGINE=wayland_egl
      export SDL_VIDEODRIVER=wayland
      export _JAVA_AWT_WM_NONREPARENTING=1
      export NO_AT_BRIDGE=1
      #systemd-cat --identifier=sway sway $@
    '';
  };


}
