## Why

The arr apps, download clients, and Flaresolverr are tightly coupled and must migrate as a group. Jellyseerr is new, replacing Ombi. This deploys the entire media pipeline to k8s.

## What Changes

- Create mediastack static NFS PV and PVC (k8s)
- Deploy Radarr, Sonarr, Lidarr, Readarr, Prowlarr, Flaresolverr to `media` namespace (k8s)
- Deploy Deluge and NZBget with mediastack volume (k8s)
- Deploy Jellyseerr (k8s)
- Add IngressRouteTCP for Deluge peer traffic on port 6881 (k8s)
- Migrate app state via rsync from NixOS to k8s PVCs (manual)
- Configure download client paths and root folders in arr apps (manual, web UI)

## Capabilities

### New Capabilities
_(none — migration of existing services plus Jellyseerr replacing Ombi)_

### Modified Capabilities
_(none)_

## Impact

- **k8s cluster repo**: Media namespace deployments using selfhosted.libsonnet + storage.libsonnet, PV/PVC, IngressRoutes, Traefik TCP route
- **Data**: Requires rsync migration and web UI reconfiguration
