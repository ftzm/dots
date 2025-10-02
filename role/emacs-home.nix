{pkgs, ...}: let
  package = pkgs.emacs-pgtk.override {};
  inherit ((pkgs.emacsPackagesFor package)) emacsWithPackages;
in {
  home.packages = [
    (emacsWithPackages (epkgs: [
      epkgs.treesit-grammars.with-all-grammars
      pkgs.python3
    ]))
  ];
}
