{ pkgs, config, lib, ... }:

# let
#   nixpkgs-steam = import (import ../nix/sources.nix).nixpkgs-steam {
#     config = {
#       allowUnfree = true;
#       checkMeta = true;
#     };
#   };
#   in

rec {
  home.packages = with pkgs; [
    # fun
    # nixpkgs-steam.steam
    steam
    discord
  ];
  personal.font_size = 10.5;
  personal.rofi_font_size = toString config.personal.font_size;
  personal.alacritty_font_size = config.personal.font_size - 4;
  personal.cursor_size = 32;
}
