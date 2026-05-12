## 1. Traefik entrypoints

- [x] 1.1 Add `wgweb` entrypoint (10.0.100.4:80) to Traefik config (k8s) (done in add-host-observability)
- [x] 1.2 Add `wgsecure` entrypoint (10.0.100.4:443) to Traefik config (k8s) (done in add-host-observability)
- [x] 1.3 Add `torrent` entrypoint (:6881) to Traefik config (k8s)

## 2. NixOS service routing

- [x] 2.1 Create Service + static Endpoints for Jellyfin (192.168.1.4:8096) (k8s)
- [x] 2.2 Create public IngressRoute for Jellyfin (k8s)
- [x] 2.3 Create Service + static Endpoints for WebDAV (k8s)
- [x] 2.4 Create private IngressRoute for WebDAV (k8s)

## 3. All service IngressRoutes

- [x] 3.1 Create private IngressRoutes for all remaining services (k8s)
- [x] 3.2 Verify all services accessible via Traefik
