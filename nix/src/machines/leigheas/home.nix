{ pkgs, config, ... }:

let secrets = import ../../secrets.nix;
in {
  home.packages = with pkgs; [ awscli lsb-release discord ];
  personal.font_size = 10.0;
  personal.rofi_font_size = "20";
  personal.alacritty_font_size = 6.5;
  personal.cursor_size = 128;
  personal.zsh_extra = ''
  '';

  services.mpd = {
    enable = true;
    dbFile = null;
    musicDirectory = "/mnt/music";
    extraConfig = ''
      database {
          plugin  "proxy"
          host    "10.0.100.4"
          port    "6600"
      }
      audio_output {
        type "pulse"
        name "Pulseaudio"
      }
    '';
  };
}
