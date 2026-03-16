{
  pkgs,
  lib,
  config,
  ...
}: {
  # So we have basic fonts setup
  fonts.enableDefaultPackages = lib.mkDefault true;

  # Import shared sway home-manager module
  home-manager.users.ftzm.imports = [
    ./sway-home.nix
  ];

  # interface.nix-specific sway overrides
  hm.wayland.windowManager.sway.config.keybindings = let
    modifier = "Mod4";
  in
    lib.mkOptionDefault {
      "${modifier}+space" = "exec ${pkgs.tofi}/bin/tofi-drun --drun-launch=true";
    };

  hm.home.pointerCursor = {
    name = "Vanilla-DMZ";
    package = pkgs.vanilla-dmz;
    size = 16;
    gtk.enable = true;
  };

  #needed for sway to work
  services.dbus.enable = true;
  security.polkit.enable = true;

  #needed for swaylock to work
  security.pam.services.swaylock = {};
  #needed for gtklock to work
  security.pam.services.gtklock = {};

  # wayland screensharing
  xdg.portal = {
    enable = true;
    config = {
      common = {
        default = "wlr";
      };
    };
    wlr.enable = true;
    wlr.settings.screencast = {
      output_name = "eDP-1";
      chooser_type = "simple";
      chooser_cmd = "${pkgs.slurp}/bin/slurp -f %o -or";
    };
  };
}
