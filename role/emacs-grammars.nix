# tree-sitter grammar bundle with a Chez-corrected scheme grammar.
#
# The upstream tree-sitter-scheme grammar follows R7RS bar-symbol escaping,
# where `\|` is an escaped pipe that still needs a closing `|`.  Chez instead
# reads `|\|` as the symbol `\` (backslash is a literal char inside `|...|`,
# the next `|` closes).  With the upstream grammar, a `|\|` in Chez source
# (e.g. s/syntax.ss) is parsed as an unterminated symbol, so the rest of the
# file mis-parses and highlighting / indentation / structural motion break.
#
# We rebuild only the scheme grammar from a Chez-corrected grammar.js and swap
# it into the otherwise-unchanged bundle.
{
  pkgs,
  epkgs,
}: let
  schemeChez = pkgs.tree-sitter.buildGrammar {
    language = "scheme";
    version = "6cdh-chez";
    src = pkgs.applyPatches {
      src = pkgs.tree-sitter-grammars.tree-sitter-scheme.src;
      patches = [./tree-sitter-scheme-chez.patch];
    };
    generate = true;
  };
in
  (epkgs.treesit-grammars.with-all-grammars).overrideAttrs (old: {
    buildCommand =
      (old.buildCommand or "")
      + ''
        rm -f $out/lib/libtree-sitter-scheme.so
        ln -s ${schemeChez}/parser $out/lib/libtree-sitter-scheme.so
      '';
  })
