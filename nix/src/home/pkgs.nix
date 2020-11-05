let
  packages = import ../overlays ;
in
  import (import ../nix/sources.nix).nixpkgs-unstable {
    config = {
      allowUnfree = true;
      checkMeta = true;
      permittedInsecurePackages = [
         "p7zip-16.02"
       ];
    };
  }
