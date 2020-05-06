{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    gp-saml-gui
    openconnect
  ];
  personal.font_size = 6.5;
}
