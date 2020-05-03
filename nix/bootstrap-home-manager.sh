NIXPKGS=$(nix eval --raw '(import ./src/nix/sources.nix).nixpkgs-unstable.outPath')
NIX_PATH=nixpkgs=$NIXPKGS
nix-shell -E 'import (import ./src/nix/sources.nix).home-manager' -A install
