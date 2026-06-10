{pkgs, ...}: let
  package = pkgs.emacs-pgtk.override {};
  epkgs = pkgs.emacsPackagesFor package;
  # tree-sitter bundle with a Chez-corrected scheme grammar (see emacs-grammars.nix)
  schemeGrammars = import ./emacs-grammars.nix {inherit pkgs epkgs;};
in {
  home.packages = [
    (epkgs.emacsWithPackages (e: [
      schemeGrammars
      e.vterm
    ]))
    # pkgs.python3
    pkgs.emacs-lsp-booster
  ];
}
