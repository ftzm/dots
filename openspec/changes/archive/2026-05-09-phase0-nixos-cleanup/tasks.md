## 1. Remove dead OCI containers

- [x] 1.1 Remove PhotoView container, photoview-db container, and create-photoview-network systemd service
- [x] 1.2 Remove Pigallery2 container
- [x] 1.3 Remove Filebrowser container

## 2. Remove dead services

- [x] 2.1 Remove Ombi service definition and its nginx virtualHost
- [x] 2.2 Remove Muscleup nginx virtualHost and static file root reference

## 3. Remove dead monitoring config

- [x] 3.1 Remove disabled Prometheus config block (exporters and scrapeConfigs)

## 4. Verify

- [x] 4.1 Run `nix eval` or build to confirm the nuc config still evaluates cleanly
