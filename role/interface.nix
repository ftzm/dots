{
  pkgs,
  lib,
  config,
  ...
}: {
  # So we have basic fonts setup
  fonts.enableDefaultPackages = lib.mkDefault true;

  hm.wayland.windowManager.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
    config = {
      bars = [
        {
          command = "waybar";
          mode = "invisible";
          # hiddenState = "hide";
          # extraConfig = ''
          #   modifier Mod4
          # '';
        }
      ];
      startup = [
        {
          command = "swaymsg output eDP-1 scale 1";
          always = true;
        }
      ];
      modifier = "Mod4";
      terminal = "foot";
      gaps = {
        smartBorders = "on";
        # smartGaps = true;
        # inner = 10;
      };
      window = {
        titlebar = false;
        border = 3;
      };
      keybindings = let
        modifier =
          config.hm.wayland.windowManager.sway.config.modifier;
      in
        lib.mkOptionDefault {
          "${modifier}+Shift+r" = "reload";
          #"${modifier}+Shift+c" = "kill";
          "${modifier}+space" = "exec ${pkgs.tofi}/bin/tofi-drun --drun-launch=true -c ~/.config/tofi/dmenu";
          # "${modifier}+Shift+b" = "exec splitv";
          "${modifier}+v" = "exec volumectl -p -u up";
          "${modifier}+Shift+v" = "exec volumectl -p -u down";
          "${modifier}+F1" = "exec volumectl toggle-mute";
          "${modifier}+p" = "exec mpc toggle";
          # "${modifier}+Shift+z" = "exec fzf_key.sh";
          "${modifier}+s" = ''mode "system"'';
          # "${modifier}+o" = ''mode "org"'';
          "${modifier}+f" = "exec dired.sh";
          "${modifier}+Shift+z" = "exec clip_key";
          "${modifier}+m" = "exec '[ \"$(swaymsg -t get_bar_config bar-0 | jq -r \".mode\")\" = \"dock\" ] && swaymsg bar mode invisible || swaymsg bar mode dock'";
        };
      modes.system = let
        md = "swaymsg mode default;";
        brightness = let
          toKey = n: {
            name = toString n;
            value = "exec lightctl set ${toString (n + 1)}0%; mode default";
          };
        in
          builtins.listToAttrs (map toKey (lib.lists.range 0 9));
      in
        {
          # "l" = ''exec "${md}${swaylockCmd}"'';
          # "d" = ''exec "${md}echo '?' > /tmp/statuspipe.fifo"'';
          "Escape" = "mode default";
          "Return" = "mode default";
        }
        // brightness;
      # modes.org = let
      #   md = "swaymsg mode default;";
      # in {
      #   "t" = ''exec "${md}capture.sh t"'';
      #   "w" = ''exec "${md}capture.sh w"'';
      #   "Escape" = "mode default";
      #   "Return" = "mode default";
      # };
      colors = {
        focused = {
          background = "#5f676a";
          border = "#000000";
          childBorder = "#458588";
          indicator = "#484e50";
          text = "#ffffff";
        };
        unfocused = {
          background = "#5f676a";
          border = "#000000";
          childBorder = "#282828";
          indicator = "#484e50";
          text = "#ffffff";
        };
      };
    };
    extraConfig = ''
      # for_window [title="capture"] floating enable, resize set 660 300
      # for_window [title="capture"] border pixel 1
      # for_window [app_id="foot-launcher"] floating enable, resize set 400 300
      # for_window [app_id="foot-launcher"] border pixel 1

      # default_border pixel 0

      # smart_gaps on
      # gaps inner 1
      # gaps outer -1


      # hide_edge_borders smart
      seat seat0 xcursor_theme Vanilla-DMZ 64

      set $TBALL1 1149:32792:Kensington_Expert_Wireless_TB_Mouse
      input $TBALL1 {
        scroll_method on_button_down
        scroll_button 275
        scroll_factor 0.3
      }

      set $TBALL2 1149:32793:ExpertBT5.0_Mouse
      input $TBALL2 {
        scroll_method on_button_down
        scroll_button 275
        scroll_factor 0.3
      }

      # exec systemctl --user import-environment XDG_SESSION_TYPE XDG_CURRENT_DESKTOP
      # exec dbus-update-activation-environment WAYLAND_DISPLAY

    '';
  };

  hm.programs.waybar = {
    enable = true;
    settings = [
      {
        "bar_id" = "bar-0";
        "ipc" = true;
        "modules-left" = [
          "sway/workspaces"
          "sway/mode"
          "sway/scratchpad"
        ];
        "modules-center" = ["sway/window"];
        "modules-right" = [
          "mpd"
          "idle_inhibitor"
          "pulseaudio"
          "network"
          "cpu"
          "memory"
          "temperature"
          "backlight"
          "keyboard-state"
          "sway/language"
          "battery"
          "clock"
          "tray"
        ];
        "position" = "bottom";
      }
    ];
  };

  hm.programs.foot = {
    enable = true;
    settings = {
      main = {
        font = "iosevka ftzm:medium:size=17";
        # letter-spacing = "-0.2";
      };
      cursor = {color = "282828 ebdbb2";};
      colors = {
        background = "282828";
        foreground = "ebdbb2";
        regular0 = "282828";
        regular1 = "cc241d";
        regular2 = "98971a";
        regular3 = "d79921";
        regular4 = "458588";
        regular5 = "b16286";
        regular6 = "689d6a";
        regular7 = "a89984";
        bright0 = "928374";
        bright1 = "fb4934";
        bright2 = "b8bb26";
        bright3 = "fabd2f";
        bright4 = "83a598";
        bright5 = "d3869b";
        bright6 = "8ec07c";
        bright7 = "ebdbb2";
      };
    };
  };

  hm.home.pointerCursor = {
    name = "Vanilla-DMZ";
    package = pkgs.vanilla-dmz;
    size = 16;
    gtk.enable = true;
  };

  hm.services.avizo = {
    enable = true;
    settings = {
      default = {
        time = 1.0;
        y-offset = 0.5;
        fade-in = 0.1;
        fade-out = 0.2;
      };
    };
  };

  #needed for sway to work
  services.dbus.enable = true;
  security.polkit.enable = true;

  #needed for swaylock to work
  security.pam.services.swaylock = {};
  #needed for gtklock to work
  security.pam.services.gtklock = {};

  # screen sharing with sway
  xdg.portal = {
    enable = true;
    wlr.enable = true; # adds pkgs.xdg-desktop-portal-wlr to extraPortals
    extraPortals = [
      pkgs.xdg-desktop-portal-gtk # gtk portal needed to make gtk apps happy
    ];
  };
}
