{ config, pkgs, lib, ... }:
let
  font_size_float = config.personal.font_size;
  font_size = toString font_size_float;
in {
  wayland.windowManager.sway = {
    enable = true;
    extraSessionCommands = ''
      # SDL:
      export SDL_VIDEODRIVER=wayland
      # QT (needs qt5.qtwayland in systemPackages):
      export QT_QPA_PLATFORM=wayland-egl
      export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"
      # Fix for some Java AWT applications (e.g. Android Studio),
      # use this if they aren't displayed properly:
      export _JAVA_AWT_WM_NONREPARENTING=1
      # Firefox
      export MOZ_ENABLE_WAYLAND=1
      #GTK
      export GDK_BACKEND=wayland
      # other
      export XDG_SESSION_TYPE=wayland
      # For hotplugging monitors
      export WLR_DRM_NO_MODIFIERS=1
      # to get a cursor
      export WLR_NO_HARDWARE_CURSORS=1
    '';
    systemdIntegration = true;
    config = {
      terminal = "foot";
      modifier = "Mod4";
      bars = [ ];
      startup = [
        {
          command = "amixer set Master mute";
          always = true;
        }
        { command = "sway_workspace_dump.sh"; }
        { command = "kanshi"; }
        { command = "mpDris2"; }
      ];
      keybindings =
        let modifier = config.wayland.windowManager.sway.config.modifier;
        in lib.mkOptionDefault {
          "${modifier}+Shift+r" = "reload";
          "${modifier}+Shift+c" = "kill";
          "${modifier}+space" = "exec my_dmenu.sh | xargs swaymsg exec --";
          "${modifier}+Shift+b" = "exec splitv";
          "${modifier}+v" = "exec panel_volume +";
          "${modifier}+Shift+v" = "exec panel_volume -";
          "${modifier}+F1" = "exec amixer set Master toggle";
          "${modifier}+p" = "exec mpc toggle";
          "${modifier}+Shift+z" = "exec clip_key";
          "${modifier}+s" = ''mode "system"'';
        };
      modes.system = let
        md = "swaymsg mode default;";
        swaylockCmd = ''
          swaylock -S \
            --effect-pixelate 10 \
            --effect-greyscale \
            --indicator \
            --clock \
            --ring-color \#000000 \
            --key-hl-color \#FFFFFF \
            --text-color \#FFFFFF \
        '';
        brightness = let
          range = [ 0 1 2 3 4 5 6 7 8 9 ];
          toKey = n: {
            name = toString n;
            value = "exec light -S ${toString (n + 1)}0";
          };
        in builtins.listToAttrs (map toKey range);
      in {
        "l" = ''exec "${md}${swaylockCmd}"'';
        "d" = ''exec "${md}echo '?' > /tmp/statuspipe.fifo"'';
        "Escape" = "mode default";
        "Return" = "mode default";
      } // brightness;
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
      default_border pixel

      hide_edge_borders smart
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
  home.packages = with pkgs; [
    (swaylock-effects.overrideAttrs (old: {
      src = fetchFromGitHub {
        owner = "mortie";
        repo = "swaylock-effects";
        rev = "a8fc557b86e70f2f7a30ca9ff9b3124f89e7f204";
        sha256 = "0f9571blnn7lg317js1j1spc5smz69i5aw6zkhskkm5m633rrpqq";
      };
    }))
    swayidle
    xwayland # for legacy apps
    waybar # status bar
    mako # notification daemon
    kanshi # autorandr
    wofi
    dmenu-wayland
    swaybg
    mpdris2 # mpd shtuff
    wdisplays
  ];
  services.dunst = {
    enable =
      true; # enabled to write config, but service fails when running sway as user
    waylandDisplay = "wayland-1";
    settings = {
      global = {
        offset = "20x20";
        width = "(0, 300)";
        transparency = 0;
        notification_height = 0;
        separator_height = 3;
        padding = 12;
        horizontal_padding = 12;
        frame_width = 3;
        frame_color = "#bdae93";
        separator_color = "frame";
        idle_threshold = 120;
        font = "Iosevka Lig Medium ${toString (font_size_float + 2)}";
        line_height = 0;
        markup = "full";
        #format = ''<b>%s</b>\n%b'';
        alignment = "right";
        show_age_threshold = 60;
        word_wrap = "yes";
        ellipsize = "middle";
        ignore_newline = "no";
        stack_duplicates = "true";
        hide_duplicate_count = false;
        show_indicators = "yes";
        icon_position = "off";
        sticky_history = "yes";
        history_length = 20;
        follow = "keyboard";
      };
      pipestatus = {
        appname = "pipestatus";
        format = "%b";
      };
      urgency_low = {
        background = "#222222";
        foreground = "#888888";
        timeout = 10;
      };
      urgency_normal = {
        background = "#282828";
        foreground = "#ebdbb2";
        timeout = 10;
      };
      urgency_critical = {
        background = "#900000";
        foreground = "#ffffff";
        frame_color = "#ff0000";
        timeout = 0;
      };
    };
  };
  programs.foot = {
    enable = true;
    settings = {
      main = {
        font = "Iosevka Lig:medium:size=12";
        line-height = "17"; # This is arbitrary but matches the height I had tweaked in emacs
        dpi-aware = "no";
      };
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
  systemd.user.services.pipestatus = {
    Unit = {
      Description = "pipestatus";
      After = [ "graphical-session-pre.target" ];
      PartOf = [ "graphical-session.target" ];
    };
    Service = {
      ExecStart =
        "${pkgs.pipestatus.pipestatus-wrapped}/bin/pipestatus-wrapped";
      ExecReload = "kill -SIGUSR2 $MAINPID";
      Restart = "on-failure";
      KillMode = "mixed";
      PrivateTmp = "false";
    };
    Install = { WantedBy = [ "graphical-session.target" ]; };
  };
}
