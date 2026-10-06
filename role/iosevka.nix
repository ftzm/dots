{pkgs, ...}: let
  # Prebuilt release of github.com/ftzm/iosevka-ftzm; never built here.
  ios = pkgs.callPackage ../pkgs/iosevka-ftzm.nix {};
in {
  fonts.fontconfig.localConf = builtins.readFile ./symbols-iosevka.conf;
  fonts.packages = with pkgs; [
    ios
    nerd-fonts.symbols-only
    (pkgs.callPackage ./symbols-iosevka.nix {})
  ];
}
