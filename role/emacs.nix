{pkgs, ...}: let
  package = pkgs.emacs-pgtk.override {};
  inherit ((pkgs.emacsPackagesFor package)) emacsWithPackages;
in {
  environment.systemPackages = [
    (emacsWithPackages (epkgs: [
      epkgs.treesit-grammars.with-all-grammars
      #epkgs.tree-sitter
      #epkgs.tree-sitter-langs
      #epkgs.telega
      epkgs.vterm
      #epkgs.emms
      #emms-taglib
      #epkgs.org
      #epkgs.pdf-tools
      pkgs.python3
    ]))
  ];
}
