{ config, pkgs, lib, ... }:
let
  font_size_float = config.personal.font_size;
  font_size = toString font_size_float;
in {
  # X
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
    '';
  # pointerCursor = {
    # package = pkgs.vanilla-dmz;
    # name = "Vanilla-DMZ";
    # size = config.personal.cursor_size;
  # };
  };
  programs.alacritty = {
    enable = true;
    settings = {
      env = { WINIT_X11_SCALE_FACTOR = "2.2"; };
      font = {
        normal = {
          family = "Iosevka Lig";
          style = "Medium";
        };
        bold = {
          family = "Iosevka Lig";
          style = "Bold";
        };
        size = 10;
      };
      colors = {
        primary = {
          background = "0x282828";
          foreground = "0xebdbb2";
        };
        normal = {
          black = "0x282828";
          red = "0xcc241d";
          green = "0x98971a";
          yellow = "0xd79921";
          blue = "0x458588";
          magenta = "0xb16286";
          cyan = "0x689d6a";
          white = "0xa89984";
        };
        bright = {
          black = "0x928374";
          red = "0xfb4934";
          green = "0xb8bb26";
          yellow = "0xfabd2f";
          blue = "0x83a598";
          magenta = "0xd3869b";
          cyan = "0x8ec07c";
          white = "0xebdbb2";
        };
      };
    };
  };
}
