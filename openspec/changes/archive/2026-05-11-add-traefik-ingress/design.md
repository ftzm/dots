## Context

Traefik needs WG-bound entrypoints and IngressRoutes for all services before we can remove nginx. NixOS services (Jellyfin, WebDAV) that stay on the host need static k8s Endpoints pointing to the host IP.

## Goals / Non-Goals

**Goals:**
- All services routable through Traefik
- WG/Tailscale traffic hits Traefik directly (no nginx intermediary)
- Jellyfin on public entrypoints, everything else private

**Non-Goals:**
- Removing nginx (separate change)
- Migrating any services to k8s

## Decisions

**Static Endpoints for NixOS services.** ExternalName doesn't work with IngressRoutes. Static Endpoints pointing to 192.168.1.4 work because Traefik uses hostNetwork.

**Separate WG entrypoints bound to 10.0.100.4.** Keeps private and public traffic on separate listen addresses.

## Risks / Trade-offs

**Risk: Duplicate routing during overlap period** → Acceptable. Both nginx and Traefik can route to the same backends simultaneously. No conflict.
