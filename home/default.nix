{ pkgs, lib, ... }:
let
in {
  imports = [
    ./personal-options.nix
    ./packages.nix
    ./services.nix
    ./shell.nix
    ./development.nix
    ./mail.nix
    ./pipestatus.nix
    ./wayland.nix
    ./X.nix
    ./notification.nix
  ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  programs.home-manager.path = "/home/ftzm/home-manager"; # can be any string; requred but unsused
}
