{ config, pkgs, ... }:

let
  package = pkgs.emacsPgtkGcc;
  emacsWithPackages = (pkgs.emacsPackagesNgGen package).emacsWithPackages;
  emms-taglib = pkgs.stdenv.mkDerivation {
    name = "emms-taglib";
    src = pkgs.fetchurl {
      url = "ftp://ftp.gnu.org/gnu/emms/emms-5.4.tar.gz";
      sha256 = "1nd7sb6pva7qb1ki6w0zhd6zvqzd7742kaqi0f3v4as5jh09l6nr";
    };

    buildInputs = [
      pkgs.zlib
      pkgs.taglib
    ];
    buildPhase = "make emms-print-metadata";
    installPhase = ''
      mkdir -p $out/bin
      cp src/emms-print-metadata $out/bin
    '';

    meta = with pkgs.lib; {
      description = "EMMS TagLib shim";
      homepage = "https://www.gnu.org/software/emms/";
      license = licenses.gpl3Plus;
      #   maintainers = with maintainers; [ your-name-here ];
      platforms = platforms.linux ++ platforms.darwin;
    };
  };
in

emacsWithPackages (epkgs: [
  #epkgs.telega
  #epkgs.vterm
  #epkgs.emms
  #emms-taglib
  #epkgs.org
  #epkgs.pdf-tools
  pkgs.python3
])
