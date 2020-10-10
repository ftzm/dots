{ config, pkgs, ... }:
let
  font_size_float = config.personal.font_size;
  font_size = toString font_size_float;
in
{
  services.dunst = {
    enable = true;
    settings = {
      global = {
        geometry = "0x5-20+20";
        transparency = 0;
        notification_height = 0;
        separator_height = 3;
        padding = 12;
        horizontal_padding = 12;
        frame_width = 3;
        frame_color = "#bdae93";
        separator_color = "frame";
        idle_threshold = 120;
        font = "Iosevka Lig Medium ${font_size}";
        line_height = 0;
        markup = "full";
        format = ''
          <b>%s</b>
          %b'';
          alignment = "left";
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
  programs.alacritty = {
    enable = true;
    settings = {
      font = {
        normal = {
          family = "Iosevka Lig";
          style = "Medium";
        };
        bold = {
          family = "Iosevka Lig";
          style = "Bold";
        };
        size = config.personal.alacritty_font_size;
      };
      colors = {
        primary = {
          background = "0x282828";
          foreground = "0xebdbb2";
        };
        normal = {
          black =   "0x282828";
          red =     "0xcc241d";
          green =   "0x98971a";
          yellow =  "0xd79921";
          blue =    "0x458588";
          magenta = "0xb16286";
          cyan =    "0x689d6a";
          white =   "0xa89984";
        };
        bright = {
          black =   "0x928374";
          red =     "0xfb4934";
          green =   "0xb8bb26";
          yellow =  "0xfabd2f";
          blue =    "0x83a598";
          magenta = "0xd3869b";
          cyan =    "0x8ec07c";
          white =   "0xebdbb2";
        };
      };
    };
  };
  programs.urxvt = {
    enable = true;
    fonts = [
      #"xft:Iosevka Lig:Medium:size=${font_size}"
      "xft:Source Code Pro:Regular:size=${font_size}"
      #"xft:DejaVu Sans Mono:Regular:size=${font_size}"
    ];
    extraConfig = {
      scrollBar = false;
      letterSpace = ".5";
      smoothResize = true;
      interalBorder = 5;
      urgentOnBell = true;
      #boldFont = "xft:Iosevka Lig:Medium:size=${font_size}";
      boldFont = "xft:Source Code Pro:Regular:size=${font_size}";
    };
  };
  xresources.properties = {
    "*background" = "#282828";
    "*foreground" = "#ebdbb2";
    "*color0" = "#282828";
    "*color8" = "#928374";
    "*color1" = "#cc241d";
    "*color9" = "#fb4934";
    "*color2" = "#98971a";
    "*color10" = "#b8bb26";
    "*color3" = "#d79921";
    "*color11" = "#fabd2f";
    "*color4" = "#458588";
    "*color12" = "#83a598";
    "*color5" = "#b16286";
    "*color13" = "#d3869b";
    "*color6" = "#689d6a";
    "*color14" = "#8ec07c";
    "*color7" = "#a89984";
    "*color15" = "#ebdbb2";
    "rofi.hide-scrollbar" = "true";
    "rofi.font" = "Iosevka Lig Medium ${config.personal.rofi_font_size}";
    "rofi.bw" = "2";
    "rofi.separator-style" = "none";
    "rofi.color-window" = "#282828, #ebdbb2, #282828";
    "rofi.color-normal" = "#282828, #ebdbb2, #282828, #b8bb26, #282828";
  };
  xsession = {
    enable = true;
    scriptPath = ".hm-session";
    windowManager = {
      xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };
    };
    initExtra = ''
      export LOCALE_ARCHIVE_2_27="$(nix-build --no-out-link "<nixpkgs>" -A glibcLocales)/lib/locale/locale-archive"
      ${pkgs.hsetroot}/bin/hsetroot -solid "#282828" &
      export FONT_SIZE=${font_size}
      xsetroot -cursor_name left_ptr
      # Mainly for ubuntu
      export XCURSOR_PATH=$HOME/.nix-profile/share/icons:$XCURSOR_PATH
      PATH=$HOME/bin:$PATH
    '';
    pointerCursor = {
      package = pkgs.vanilla-dmz;
      name = "Vanilla-DMZ";
      size = config.personal.cursor_size;
    };
  };
  qt.enable = true;
  gtk = {
    enable = true;
    font.name = "Iosevka Lig Medium ${font_size}";
  };
  fonts.fontconfig.enable = true;
}
