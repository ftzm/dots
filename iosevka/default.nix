{ iosevkaPkgs, ... }:

let iosevkaOrig = iosevkaPkgs.iosevka.override {
  privateBuildPlan = {
    weights.regular = {
      shape = 500;
      menu = 500;
      css = 500;
    };
    weights.medium = {
      shape = 600;
      menu = 600;
      css = 600;
    };
    weights.semibold = {
      shape = 600;
      menu = 600;
      css = 600;
    };
    weights.bold = {
      shape = 700;
      menu = 700;
      css = 700;
    };
    ligations = {
      inherits = "haskell";
    };
    variants.design = {
      asterisk = "hex-low";
      dollar = "open-cap";
      percent = "dots";
    };
    family = "Iosevka Lig";
  };
  set = "ftzm";
  extraParameters = builtins.readFile ./iosevka.toml;
    };
  in
iosevkaPkgs.stdenv.mkDerivation {
  name = "iosevka-nerd";
  dontUnpack = true;
  buildPhase = ''
    shopt -s globstar
    cp -r ${iosevkaOrig} .
    for file in **/*.ttf; do
      ${iosevkaPkgs.nerd-font-patcher}/bin/nerd-font-patcher \
        -c \
        "$file"
    done
  '';
  installPhase = ''
    cp -r . $out
  '';
}

#   overrideAttrs(oldAttrs: {
#     buildPhase = ''
#     export HOME=$TMPDIR
#     runHook preBuild
#     ${pkgs.nerd-font-patcher/bin/nerd-font-patcher} *.otf
#     ${pkgs.nerd-font-patcher/bin/nerd-font-patcher} *.ttf
#     npm run build --no-update-notifier -- --jCmd=$NIX_BUILD_CORES --verbose=9 ttf::$pname
#     runHook postBuild
#   '';
# }
# )
