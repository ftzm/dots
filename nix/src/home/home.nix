{ config, lib, ... }:

let
  packages = import ../overlays ;
  iosevka = import ../overlays/iosevka/iosevka.nix;
  home_manager_repo = (import ../nix/sources.nix).home-manager.outPath;
  pkgs = import (import ../nix/sources.nix).nixpkgs-unstable {
    overlays = [ packages iosevka ];
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
  programs.home-manager.path = "${home_manager_repo}";
}
