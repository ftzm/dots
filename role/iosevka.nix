{
  inputs,
  pkgs,
  ...
}: {
  fonts.packages = with pkgs; [
    jetbrains-mono
    (let
      iosevkaPkgs = inputs.nixpkgs-iosevka.legacyPackages.x86_64-linux;
    in
      iosevkaPkgs.iosevka.override {
        privateBuildPlan = builtins.readFile ./iosevka-build-plan.toml;
        extraParameters = builtins.readFile ./iosevka.toml;
        set = "-ftzm";
      })
  ];
}
