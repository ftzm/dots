## Why

Services need Traefik IngressRoutes before we can remove the nginx proxy layer. This change creates all the k8s-side routing so traffic can flow through Traefik instead of nginx.

## What Changes

- Add WG-bound Traefik entrypoints (`wgweb`/`wgsecure` on 10.0.100.4, `torrent` on :6881) (k8s)
- Create Service + static Endpoints for Jellyfin (192.168.1.4:8096) (k8s)
- Create IngressRoutes for all services using wildcard cert (k8s)
- Jellyfin on public entrypoints; everything else private

## Capabilities

### New Capabilities
_(none — infrastructure routing)_

### Modified Capabilities
_(none)_

## Impact

- **k8s cluster repo**: Traefik config, IngressRoutes, Service+Endpoints for NixOS-hosted services
