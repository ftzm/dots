{model}: { pkgs, lib, ... }:
let
  conditional_imports =
    lib.attrByPath [ model ] [ ] {
      "ThinkPad T480s" = [ ./personal.nix ];
      "ThinkPad X1 Extreme 2nd" = [ ./unity.nix ];
    };
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
  ] ++ conditional_imports;

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  programs.home-manager.path = "/home/matt/home-manager"; # can be any string; requred but unsused
}
