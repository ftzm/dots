{ extra-imports ? [] }: { pkgs, lib, ... }:
let
in {
  # nixpkgs.config = { firefox = { enableTridactylNative = true; }; };
  imports = [
    ./personal-options.nix
    ./packages.nix
    ./services.nix
    ./shell.nix
    ./development.nix
    ./mail.nix
    ./xorg.nix
    ./pipestatus.nix
  ] ++ extra-imports;

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  programs.home-manager.path = "/home/ftzm/home-manager"; # can be any string; requred but unsused
}
