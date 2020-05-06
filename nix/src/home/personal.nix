{ config, lib, ... }:

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
    pkgs-old.steam
    discord
  ];
  personal.font_size = 10.5
}
