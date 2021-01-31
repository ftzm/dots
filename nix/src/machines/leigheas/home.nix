{ pkgs, config, ... }:

let
  secrets = import ../../secrets.nix;
in {
  home.packages = with pkgs; [
    awscli
    lsb-release
  ];
  personal.font_size = 10.0;
  personal.rofi_font_size = "20";
  personal.alacritty_font_size = 6.5;
  personal.cursor_size = 128;
  personal.zsh_extra = ''
    ${secrets.work.devdb}
  '';
}
