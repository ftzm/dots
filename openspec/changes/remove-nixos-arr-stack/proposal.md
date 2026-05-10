## Why

With the arr stack running in k8s and verified (done in deploy-arr-stack), remove the NixOS service definitions. Also update Jellyfin library paths to point at the new mediastack mount.

## What Changes

- Remove Radarr, Sonarr, Lidarr, Readarr, Prowlarr services from nuc config
- Remove Deluge and NZBget services from nuc config
- Update Jellyfin library paths to `/mnt/nas/mediastack/media/...`

## Capabilities

### New Capabilities
_(none — removal + path update)_

### Modified Capabilities
_(none)_

## Impact

- **machines/nuc/default.nix**: Remove service blocks, update Jellyfin paths
