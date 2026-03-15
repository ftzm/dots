{pkgs, ...}: let
  package = pkgs.emacs-pgtk.override {};
  inherit ((pkgs.emacsPackagesFor package)) emacsWithPackages;
in {
  home.packages = [
    (emacsWithPackages (epkgs: [
      epkgs.treesit-grammars.with-all-grammars
      epkgs.vterm
    ]))
    # pkgs.python3
    pkgs.emacs-lsp-booster
  ];
}
