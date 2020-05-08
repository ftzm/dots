{ pkgs, config, lib, ... }:

let
  pkgs-old = import (import ../nix/sources.nix).nixpkgs-old {
    config = {
      allowUnfree = true;
      checkMeta = true;
    };
  };
  in

{
  home.packages = with pkgs; [
    # fun
    steam
    discord
  ];
  personal.font_size = 10.5;
  personal.alacritty_font_size = config.personal.font_size - 4;
  personal.cursor_size = 32;
}
