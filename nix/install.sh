#!/usr/bin/env bash

sudo rm /etc/nixos/secrets.nix; sudo ln -s ~/.dots/nix/secrets.nix /etc/nixos/secrets.nix

rm -r ~/config/nixpkgs; ln -s ~/.dots/nix/nixpkgs ~/.config/nixpkgs

nix-env -riA nixpkgs.desktop
