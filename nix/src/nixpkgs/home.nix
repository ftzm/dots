{ config, lib, ... }:

let
  pkgs = import (import ./nix/sources.nix).nixpkgs {};
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
