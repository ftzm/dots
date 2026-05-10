## Context

Arr apps and download clients are tightly coupled — they talk to each other and share the mediastack volume for hardlinks. They must deploy together to the `media` namespace. Requires storage libs and NAS dirs from prior changes.

## Goals / Non-Goals

**Goals:**
- All pipeline apps running in k8s with hardlink-capable shared storage
- Deluge peer traffic routable from WAN via Traefik TCP
- App state migrated from NixOS

**Non-Goals:**
- Removing NixOS services (separate change)
- Automating arr app web UI configuration

## Decisions

**Deploy all at once, not incrementally.** Radarr needs Deluge to import downloads. Splitting would require cross-environment networking.

**All apps share the mediastack PV at `/data`.** Hardlinks require same filesystem. One PV ensures this.

## Risks / Trade-offs

**Risk: Deluge download path change breaks existing torrents** → Existing incomplete downloads may need to finish on NixOS or be re-added in k8s.

**Risk: Hardlinks don't work** → Verify with a test hardlink between `/data/downloads/` and `/data/media/` before full cutover.
