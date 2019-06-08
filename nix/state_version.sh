#!/usr/bin/env bash

set -e

VERSION_FILE="/etc/nixos/state-version.nix"

if [ -f "$VERSION_FILE" ]; then
    exit 0
fi

STATE_VERSION=$(nix-instantiate --eval -E '((import <nixpkgs> {}).callPackage /etc/nixos/configuration.nix {}).system.stateVersion')

cat <<EOF >$VERSION_FILE
{
system.stateVersion = $STATE_VERSION;
}
EOF
