{ inputs, pkgs,  ... }:
let
  iosevkaPkgs = inputs.nixpkgs-iosevka.legacyPackages.x86_64-linux;
  iosevkaLig = pkgs.callPackage ../iosevka {
    iosevkaPkgs = iosevkaPkgs;
    inherit pkgs;
  };
in {
  fonts = {
    fontconfig.enable = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [ font-awesome iosevkaLig jetbrains-mono];
  };
}
