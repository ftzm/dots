{ pkgs, config, ... }:

let secrets = import ../../secrets.nix;
in {
  home.packages = with pkgs; [ awscli lsb-release discord ];
  personal.font_size = 10.0;
  personal.rofi_font_size = "20";
  personal.alacritty_font_size = 6.5;
  personal.cursor_size = 128;
  personal.zsh_extra = ''
    ${secrets.work.devdb}
  '';

  services.mpd = {
    enable = true;
    dbFile = null;
    musicDirectory = "/mnt/music/music-library/music";
    extraConfig = ''
      database {
          plugin  "proxy"
          host    "10.0.100.1"
          port    "6600"
      }
    '';
  };
}
