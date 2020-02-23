{ config, lib, ... }:

let
  overlays = import ../overlays ;
  pkgs = import (import ../nix/sources.nix).nixpkgs-unstable {
    overlays = [ overlays ];
    config = {
      allowUnfree = true;
      checkMeta = true;
    };
  };
in {
  _module.args.pkgs = lib.mkForce pkgs;
  imports = [
    ./packages.nix
    ./services.nix
    ./shell.nix
    ./development.nix
    ./mail.nix
    ./xorg.nix
    ./pipestatus.nix
  ];
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
