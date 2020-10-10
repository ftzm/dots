let
  packages = import ../overlays ;
  iosevka = import ../overlays/iosevka/iosevka.nix;
in
  import (import ../nix/sources.nix).nixpkgs-unstable {
    overlays = [ packages iosevka ];
    config = {
      allowUnfree = true;
      checkMeta = true;
      permittedInsecurePackages = [
         "p7zip-16.02"
       ];
    };
  }
