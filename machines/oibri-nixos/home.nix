{ pkgs, config, lib, ... }:

# let
#   nixpkgs-steam = import (import ../nix/sources.nix).nixpkgs-steam {
#     config = {
#       allowUnfree = true;
#       checkMeta = true;
#     };
#   };
#   in

{
  home.packages = with pkgs; [
    # fun
    # nixpkgs-steam.steam
    # steam
    discord
    # ps
    epsxe
    ecmtools

  ];
  personal.font_size = 8.0;
  personal.rofi_font_size = toString (config.personal.font_size + 4.0);
  personal.alacritty_font_size = config.personal.font_size - 2.5;
  personal.cursor_size = 32;
}
