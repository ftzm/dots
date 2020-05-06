{ lib, ... }:

let
  pkgs = import ./pkgs.nix;
  home_manager_repo = (import ../nix/sources.nix).home-manager.outPath;
  conditional_imports = lib.attrByPath [(builtins.readFile /etc/nixos/model)] [] {
    "ThinkPad T480s" = [
      ./personal.nix
    ];
    "ThinkPad X1 Extreme 2nd" = [
      ./unity.nix
    ];
  };
in {
  imports = [
    ./personal-options.nix
    ./packages.nix
    ./services.nix
    ./shell.nix
    ./development.nix
    ./mail.nix
    ./xorg.nix
    ./pipestatus.nix
  ] ++ conditional_imports;
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  programs.home-manager.path = "${home_manager_repo}";
}
