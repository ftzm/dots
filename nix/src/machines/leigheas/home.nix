{ pkgs, config, ... }:

{
  home.packages = with pkgs; [
    gp-saml-gui
    openconnect
  ];
  personal.font_size = 10.0;
  personal.rofi_font_size = "20";
  personal.alacritty_font_size = 6.5;
  personal.cursor_size = 128;
}