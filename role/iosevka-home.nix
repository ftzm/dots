{
  inputs,
  pkgs,
  ...
}: let
  ios = let
    iosevkaPkgs = inputs.nixpkgs-iosevka.legacyPackages.x86_64-linux;
  in
    iosevkaPkgs.iosevka.override {
      privateBuildPlan = builtins.readFile ./iosevka-build-plan.toml;
      extraParameters = builtins.readFile ./iosevka.toml;
      set = "-ftzm";
    };
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
