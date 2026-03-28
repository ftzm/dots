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
    style = ''
      * {
          /* `otf-font-awesome` is required to be installed for icons */
          font-family: "Symbols Nerd Font", iosevka ftzm;
          font-weight: 500;
          font-size: 14px;
          min-height: 24px;
      }

      window#waybar {
          background-color: #282828;
          border-top: 2px solid #3c3836;
          color: #665c54;
          transition-property: background-color;
          transition-duration: .5s;
      }

      window#waybar.hidden {
          opacity: 0.2;
      }

      window#waybar.termite {
      }

      window#waybar.chromium {
          border: none;
      }

      button {
          box-shadow: inset 0 -3px transparent;
          border: none;
          border-radius: 0;
      }

      button:hover {
          background: inherit;
          box-shadow: inset 0 -3px #ffffff;
      }

      #workspaces button {
          padding: 0 5px;
          background-color: transparent;
          color: #665c54;
      }

      #workspaces button:hover {
          background: rgba(0, 0, 0, 0.2);
      }

      #workspaces button.focused {
          background-color: #3c3836;
          color: #7c6f64;
      }

      #workspaces button.urgent {
          background-color: #eb4d4b;
      }

      #mode {
          background-color: #64727D;
          box-shadow: inset 0 -3px #ffffff;
      }

      #clock,
      #battery,
      #cpu,
      #memory,
      #disk,
      #temperature,
      #backlight,
      #network,
      #pulseaudio,
      #wireplumber,
      #custom-media,
      #tray,
      #mode,
      #idle_inhibitor,
      #scratchpad,
      #mpd {
          padding: 0 10px;
      }

      #window,
      #workspaces {
          margin: 0 4px;
      }

      .modules-left > widget:first-child > #workspaces {
          margin-left: 0;
      }

      .modules-right > widget:last-child > #workspaces {
          margin-right: 0;
      }

      #battery.charging, #battery.plugged {
      }

      @keyframes blink {
          to {
              background-color: #ffffff;
              color: #000000;
          }
      }

      #battery.critical:not(.charging) {
          background-color: #f53c3c;
          animation-name: blink;
          animation-duration: 0.5s;
          animation-timing-function: steps(12);
          animation-iteration-count: infinite;
          animation-direction: alternate;
      }

      label:focus {
          background-color: #000000;
      }

      #pulseaudio.muted {
          background-color: #90b1b1;
          color: #2a5c45;
      }

      #wireplumber {
          background-color: #fff0f5;
          color: #000000;
      }

      #wireplumber.muted {
          background-color: #f53c3c;
      }

      #custom-media {
          background-color: #66cc99;
          color: #2a5c45;
          min-width: 100px;
      }

      #custom-media.custom-spotify {
          background-color: #66cc99;
      }

      #custom-media.custom-vlc {
          background-color: #ffa000;
      }

      #temperature.critical {
          background-color: #eb4d4b;
      }

      #tray {
          background-color: #2980b9;
      }

      #tray > .passive {
          -gtk-icon-effect: dim;
      }

      #tray > .needs-attention {
          -gtk-icon-effect: highlight;
          background-color: #eb4d4b;
      }

      #mpd {
      }

      #mpd.disconnected {
      }

      #mpd.stopped {
      }

      #language {
          padding: 0 5px;
          margin: 0 5px;
          min-width: 16px;
      }

      #keyboard-state {
          background: #97e1ad;
          color: #000000;
          padding: 0 0px;
          margin: 0 5px;
          min-width: 16px;
      }

      #keyboard-state > label {
          padding: 0 5px;
      }

      #keyboard-state > label.locked {
          background: rgba(0, 0, 0, 0.2);
      }

      #scratchpad {
          background: rgba(0, 0, 0, 0.2);
      }

      #scratchpad.empty {
          background-color: transparent;
      }

      #privacy {
          padding: 0;
      }

      #privacy-item {
          padding: 0 5px;
          color: white;
      }

      #privacy-item.screenshare {
          background-color: #cf5700;
      }

      #privacy-item.audio-in {
          background-color: #1ca000;
      }

      #privacy-item.audio-out {
          background-color: #0069d4;
      }
    '';
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

        "sway/scratchpad" = {
          "format" = "{icon} {count}";
          "format-icons" = ["󰏃" "󰏃"];
        };
        "mpd" = {
          "format" = "󰎆 {stateIcon} {title} - {artist}";
          "format-disconnected" = "󰎆 Disconnected";
          "format-stopped" = "󰎆 Stopped";
          "format-paused" = "󰎆 {stateIcon} {title} - {artist}";
          "state-icons" = {
            "paused" = "󰏤";
            "playing" = "󰐊";
          };
        };
        "idle_inhibitor" = {
          "format" = "{icon}";
          "format-icons" = {
            "activated" = "󰈈";
            "deactivated" = "󰈉";
          };
        };
        "pulseaudio" = {
          "format" = "{icon} {volume}%";
          "format-bluetooth" = "󰂯 {volume}%";
          "format-muted" = "󰖁 muted";
          "format-icons" = {
            "default" = ["" "" ""];
          };
        };
        "network" = {
          "format-wifi" = "󰤨 {essid}";
          "format-ethernet" = "󰈀 {ipaddr}/{cidr}";
          "format-disconnected" = "󰤭 Disconnected";
        };
        "cpu" = {
          "format" = "󰘚 {usage}%";
        };
        "memory" = {
          "format" = "󰍛 {}%";
        };
        "temperature" = {
          "critical-threshold" = 80;
          "format" = "󰔏 {temperatureC}°C";
          "format-critical" = "󰔏 {temperatureC}°C";
        };
        "backlight" = {
          "format" = "󰃠 {percent}%";
        };
        "keyboard-state" = {
          "capslock" = true;
          "format" = "{icon}";
          "format-icons" = {
            "locked" = "󰌾";
            "unlocked" = "";
          };
        };
        "sway/language" = {
          "format" = "󰌌 {short}";
        };
        "battery" = {
          "format" = "{icon} {capacity}%";
          "format-charging" = "󰂄 {capacity}%";
          "format-plugged" = "󰚥 {capacity}%";
          "format-icons" = ["󰁺" "󰁻" "󰁼" "󰁽" "󰁾"];
        };
        "clock" = {
          "format" = "󰅐 {:%H:%M}";
          "format-alt" = "󰃭 {:%Y-%m-%d}";
          "tooltip-format" = "{:%Y-%m-%d | %H:%M}";
        };
      }
    ];
  };

  programs.foot = {
    enable = true;
    settings = {
      main = {
        font = lib.mkDefault "iosevka ftzm:medium:size=17";
      };
      colors-dark = {
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
