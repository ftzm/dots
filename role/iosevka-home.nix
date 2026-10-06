{pkgs, ...}: let
  # Prebuilt release of github.com/ftzm/iosevka-ftzm; never built here.
  ios = pkgs.callPackage ../pkgs/iosevka-ftzm.nix {};
in {
  fonts.fontconfig = {
    enable = true;
    configFile.ftzm-symbols = {
      enable = true;
      source = ./symbols-iosevka.conf;
    };
  };
  home.packages = with pkgs; [
    ios
    nerd-fonts.symbols-only
    (pkgs.callPackage ./symbols-iosevka.nix {})
  ];
}
