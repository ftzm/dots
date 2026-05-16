## Why

Four simple single-container services remain: Navidrome, Audiobookshelf, The Lounge, and Filestash. All use selfhosted.libsonnet and can deploy together since they're independent.

## What Changes

- Create narrow NFS PVs for music, audiobooks, and cloud directories (k8s)
- Deploy Navidrome with read-only music mount to `navidrome` namespace (k8s)
- Deploy Audiobookshelf with read-only audiobooks mount to `audiobookshelf` namespace (k8s)
- Deploy The Lounge with config PVC to `thelounge` namespace (k8s)
- Deploy Filestash with read-write cloud mount to `filestash` namespace (k8s)
- Migrate app state via rsync for each (manual)

## Capabilities

### New Capabilities
_(none — service migration)_

### Modified Capabilities
_(none)_

## Impact

- **k8s cluster repo**: Four selfhosted.libsonnet deployments, narrow NFS PVs, IngressRoutes
- **Data**: rsync migration for Navidrome (playlists), Audiobookshelf (metadata), The Lounge (chat logs)
