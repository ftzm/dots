## Why

Dead services are cluttering the nuc NixOS config. PhotoView, Pigallery2, and Filebrowser are superseded by Immich/Filestash (Phase 4). Ombi is replaced by Jellyseerr (Phase 3). Muscleup is abandoned. Prometheus was already disabled but its config remains. Cleaning these out now reduces noise before the migration phases that follow.

## What Changes

- Remove PhotoView container, photoview-db container, and create-photoview-network systemd service
- Remove Pigallery2 container
- Remove Filebrowser container
- Remove Ombi service and its nginx virtualHost
- Remove Muscleup nginx virtualHost and static file reference
- Remove disabled Prometheus config block (exporters and scrapeConfigs)

## Capabilities

### New Capabilities

_(none — this is a pure removal change)_

### Modified Capabilities

_(none — no specs are affected by removing dead services)_

## Impact

- **File:** `machines/nuc/default.nix` — all removals are in this single file
- **Running services:** PhotoView, Pigallery2, Filebrowser, and Ombi containers/services will stop after deploy via comin
- **DNS:** Any DNS records pointing to these services become orphaned (cleanup in Phase 5)
- **nginx:** Two virtualHost blocks removed (ombi, muscleup); nginx itself stays for other services
- **Monitoring:** No impact — Prometheus was already disabled
