NIXPKGS=$(nix eval --raw '(import ./src/nixpkgs/nix/sources.nix).nixpkgs.outPath')
NIX_PATH=nixpkgs=$NIXPKGS:nixos-config=/etc/nixos/configuration.nix
nixos-rebuild switch
