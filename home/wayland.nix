{ config, pkgs, lib, ... }:
let
  font_size_float = config.personal.font_size;
  font_size = toString font_size_float;
in {
  wayland.windowManager.sway = let
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
    swayidleCmd = ''
      swayidle -w \
        timeout 120 '[ ! onHomeWifi ] && ${swaylockCmd}'
    '';
    fuzzelCmd = ''
      fuzzel \
        -f iosevkaLig \
        -I \
        -x 15 \
        -y 15 \
        -r 0 \
        -b 282828ff \
        -C 3c3836ff \
        -t ebdbb2ff \
        -s 3c3836ff \
        -S ebdbb2ff \
        -B 4
    '';
  in {
    enable = true;
    package = null;
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
        { command = swayidleCmd; }
        { command = "swaybg -c '#3c3836'"; }
      ];
      keybindings =
        let modifier = config.wayland.windowManager.sway.config.modifier;
        in lib.mkOptionDefault {
          "${modifier}+Shift+r" = "reload";
          "${modifier}+Shift+c" = "kill";
          #"${modifier}+space" = "exec fzf_launcher.sh";
          "${modifier}+space" = "exec ${fuzzelCmd}";
          "${modifier}+Shift+b" = "exec splitv";
          "${modifier}+v" = "exec panel_volume +";
          "${modifier}+Shift+v" = "exec panel_volume -";
          "${modifier}+F1" = "exec amixer set Master toggle";
          "${modifier}+p" = "exec mpc toggle";
          "${modifier}+Shift+z" = "exec fzf_key.sh";
          "${modifier}+s" = ''mode "system"'';
          "${modifier}+o" = ''mode "org"'';
          "${modifier}+f" = ''exec dired.sh'';
          "${modifier}+Shift+k" = "nop i3ipc_move next";
        };
      modes.system = let
        md = "swaymsg mode default;";
        brightness = let
          toKey = n: {
            name = toString n;
            value = "exec light -S ${toString (n + 1)}0";
          };
        in builtins.listToAttrs (map toKey (lib.lists.range 0 9));
      in {
        "l" = ''exec "${md}${swaylockCmd}"'';
        "d" = ''exec "${md}echo '?' > /tmp/statuspipe.fifo"'';
        "Escape" = "mode default";
        "Return" = "mode default";
      } // brightness;
      modes.org = let
        md = "swaymsg mode default;";
      in {
        "t" = ''exec "${md}capture.sh t"'';
        "w" = ''exec "${md}capture.sh w"'';
        "Escape" = "mode default";
        "Return" = "mode default";
      };
      colors = {
        focused = {
          background = "#5f676a";
          border = "#000000";
          childBorder = "#3c3836";
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
      for_window [title="capture"] floating enable, resize set 660 300
      for_window [title="capture"] border pixel 1
      for_window [app_id="foot-launcher"] floating enable, resize set 400 300
      for_window [app_id="foot-launcher"] border pixel 1

      default_border pixel 0

      smart_gaps on
      gaps inner 1
      gaps outer -1


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

      exec systemctl --user import-environment XDG_SESSION_TYPE XDG_CURRENT_DESKTOP
      exec dbus-update-activation-environment WAYLAND_DISPLAY

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
    fuzzel
    swaybg
    mpdris2 # mpd shtuff
    wdisplays
    flashfocus
    wl-clipboard
  ];
  programs.foot = {
    enable = true;
    settings = {
      main = {
        font = "Iosevka Lig:medium:size=12";
        line-height =
          "17"; # This is arbitrary but matches the height I had tweaked in emacs
        dpi-aware = "no";
      };
      cursor = {
        color = "282828 ebdbb2";
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

  # hotplug monitors
  services.kanshi.enable = true;
  services.kanshi.extraConfig = ''
    {
      output eDP-1 enable scale 2.0 mode 3840x2160
    }
    {
      output eDP-1 disable
      output "Dell Inc. DELL U3419W HWGQ5T2" scale 1.0 mode 3440x1440
    }
  '';
  systemd.user.services.flashfocus = {
    Unit = {
      Description = "Perform windows animations on focus";
      PartOf = [ "graphical-session.target" ];
    };
    Service = {
      ExecStart = "${pkgs.flashfocus}/bin/flashfocus -o 0.7 -t 150 -l never";
      ExecReload = "kill -SIGUSR2 $MAINPID";
      Restart = "on-failure";
      KillMode = "mixed";
      PrivateTmp = "false";
    };
    Install = {
      WantedBy = [ "graphical-session.target" ];
    };
  };
}
