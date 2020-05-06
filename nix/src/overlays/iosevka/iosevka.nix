self: super:

let
  nodeEnv = with super; super.callPackage ./node-env.nix { };
  #deps = (with super; super.callPackage ./node-packages.nix { nodeEnv = nodeEnv; }).package;
  deps = (import ./default.nix { })."iosevka-build-deps-./.";

in {
  nodePackages = super.nodePackages // {
    "iosevka-build-deps-../../data/fonts/iosevka" = deps;
  };
  iosevka = super.iosevka.overrideAttrs (old: rec {
    version = "3.0.0-rc.3";
    src = super.fetchFromGitHub {
      owner = "be5invis";
      repo = "Iosevka";
      rev = "v${version}";
      sha256 = "14xfm8hg7xidgxwzxm4f5svx5kkbjxpzn060cz7y6532mi8dywf5";
    };
    buildPhase = ''
      runHook preBuild
      npm run build --no-update-notifier -- ttf::$pname
      runHook postBuild
    '';
  });
}
