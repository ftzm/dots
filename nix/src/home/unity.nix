{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    gp-saml-gui
    openconnect
  ];
  personal.font_size = 6.5;
  personal.alacritty_font_size = config.personal.font_size;
  personal.cursor_size = 128;
}
