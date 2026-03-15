{
  config,
  pkgs,
  lib,
  ...
}: {
  wayland.windowManager.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
    config = {
      bars = [
        {
          command = "waybar";
          mode = "invisible";
        }
      ];
      startup = lib.mkDefault [
        {
          command = "swaymsg output eDP-1 scale 1";
          always = true;
        }
      ];
      fonts = {
        names = ["iosevka ftzm Medium"];
        size = 10.0;
      };
      modifier = "Mod4";
      terminal = "foot";
      gaps = {
        smartBorders = "on";
      };
      window = {
        titlebar = false;
        border = 5;
      };
      keybindings = let
        inherit (config.wayland.windowManager.sway.config) modifier;
      in
        lib.mkOptionDefault {
          "${modifier}+Shift+r" = "reload";
          "${modifier}+v" = "exec volumectl -p -u up";
          "${modifier}+Shift+v" = "exec volumectl -p -u down";
          "${modifier}+F1" = "exec volumectl toggle-mute";
          "${modifier}+p" = "exec mpc toggle";
          "${modifier}+s" = ''mode "system"'';
          "${modifier}+f" = "exec dired.sh";
          "${modifier}+Shift+z" = "exec clip_key";
          "${modifier}+m" = "exec '[ \"$(swaymsg -t get_bar_config bar-0 | jq -r \".mode\")\" = \"dock\" ] && swaymsg bar mode invisible || swaymsg bar mode dock'";
          "${modifier}+c" = "exec $HOME/dots/bin/claude-launcher";
          "${modifier}+n" = "exec makoctl dismiss";
          "${modifier}+Shift+n" = "exec makoctl invoke";
          "${modifier}+Shift+p" = "exec $HOME/dots/bin/pr-status";
          "${modifier}+o" = "exec $HOME/dots/bin/scratchpad-menu";
        };
      modes.system = let
        brightness = let
          toKey = n: {
            name = toString n;
            value = "exec lightctl set ${toString (n + 1)}0%; mode default";
          };
        in
          builtins.listToAttrs (map toKey (lib.lists.range 0 9));
      in
        {
          "Escape" = "mode default";
          "Return" = "mode default";
        }
        // brightness;
      colors = lib.mkDefault {
        focused = {
          background = "#282828";
          border = "#282828";
          childBorder = "#458588";
          indicator = "#83a598";
          text = "#ebdbb2";
        };
        focusedInactive = {
          background = "#3c3836";
          border = "#282828";
          childBorder = "#3c3836";
          indicator = "#504945";
          text = "#a89984";
        };
        unfocused = {
          background = "#3c3836";
          border = "#282828";
          childBorder = "#282828";
          indicator = "#282828";
          text = "#a89984";
        };
        urgent = {
          background = "#cc241d";
          border = "#cc241d";
          childBorder = "#cc241d";
          indicator = "#fb4934";
          text = "#ebdbb2";
        };
      };
    };
    extraConfig = ''
      for_window [app_id="foot-claude"] move to workspace scratch; workspace scratch; layout tabbed
      for_window [app_id="foot-claude"] border pixel 3

      focus_on_window_activation focus
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
    '';
  };

  programs.waybar = {
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
        ];
        "position" = "bottom";
      }
    ];
  };

  programs.foot = {
    enable = true;
    settings = {
      main = {
        font = lib.mkDefault "iosevka ftzm:medium:size=17";
      };
      colors = {
        cursor = "282828 ebdbb2";
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

  xdg.configFile."tofi/config".text = ''
    font = iosevka ftzm Medium
    font-size = 14
    outline-width = 0
    border-width = 2
    border-color = #d79921
    padding-top = 4
    padding-bottom = 4
    padding-left = 8
    padding-right = 8
    background-color = #282828
    text-color = #ebdbb2
    prompt-color = #fabd2f
    selection-color = #b8bb26
    input-color = #83a598
  '';

  programs.fuzzel = {
    enable = true;
    settings = {
      main = {
        font = "Iosevka ftzm:weight=medium:size=14";
        prompt = ">";
        horizontal-pad = 8;
        vertical-pad = 4;
        inner-pad = 0;
        icons-enabled = "no";
        match-mode = "fzf";
        line-height = 22;
      };
      colors = {
        background = "282828ff";
        text = "ebdbb2ff";
        prompt = "fabd2fff";
        input = "ebdbb2ff";
        match = "d79921ff";
        selection = "3c3836ff";
        selection-text = "b8bb26ff";
        selection-match = "d79921ff";
        border = "d79921ff";
      };
      border = {
        width = 2;
        radius = 0;
      };
    };
  };

  services.avizo = {
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
}
