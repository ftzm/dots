## Why

With Immich, Vaultwarden, Navidrome, Audiobookshelf, The Lounge, and Filestash all verified running in k8s, remove their NixOS service definitions from the nuc config.

## What Changes

- Remove Immich service from nuc config
- Remove Vaultwarden service from nuc config
- Remove Navidrome service from nuc config
- Remove Audiobookshelf service from nuc config
- Remove The Lounge service from nuc config
- Remove Filestash container from nuc config

## Capabilities

### New Capabilities
_(none — pure removal)_

### Modified Capabilities
_(none)_

## Impact

- **machines/nuc/default.nix**: Remove six service/container blocks
