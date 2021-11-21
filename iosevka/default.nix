{ pkgs, ... }:

pkgs.iosevka.override {
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
}
