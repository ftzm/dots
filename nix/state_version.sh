#!/usr/bin/env bash

set -e

VERSION_FILE="/etc/nixos/state-version.nix"

if [ -f "$VERSION_FILE" ]; then
    echo "Version file exists, skipping creation."
    exit 0
fi

STATE_VERSION=$(nix-instantiate --eval -E '((import <nixpkgs> {}).callPackage /etc/nixos/configuration.nix {}).system.stateVersion')

echo "Creating version file..."

cat <<EOF >$VERSION_FILE
{
system.stateVersion = $STATE_VERSION;
}
EOF

echo "Version file created."
