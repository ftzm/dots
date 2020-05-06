#!/usr/bin/env bash
#NIXPKGS=$(nix eval --raw '(import ./src/nix/sources.nix).nixpkgs-unstable')
#NIX_PATH=nixpkgs=$NIXPKGS
#echo "$NIX_PATH"
nix-shell -E 'import (import ./src/nix/sources.nix).home-manager' -A install
