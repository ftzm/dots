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
    design =
      [
        "ligset-haskell"
        "v-asterisk-low"
        "v-percent-dots"
        "v-dollar-open"
        "v-brace-straight"
      ];
    family = "Iosevka Lig";
  };
  set = "ftzm";
  extraParameters = builtins.readFile ./iosevka.toml;
}
