## 1. Storage

- [x] 1.1 Create mediastack static NFS PV and PVC in main.jsonnet (k8s)

## 2. Deploy apps

- [x] 2.1 Deploy Radarr with mediastack volume (k8s)
- [x] 2.2 Deploy Sonarr with mediastack volume (k8s)
- [x] 2.3 Deploy Lidarr with mediastack volume (k8s)
- [x] 2.4 Deploy Readarr with mediastack volume (k8s)
- [x] 2.5 Deploy Prowlarr (k8s)
- [x] 2.6 Deploy Flaresolverr (k8s)
- [x] 2.7 Deploy Jellyseerr (k8s)
- [x] 2.8 Update Deluge download paths to use mediastack (NixOS)
- [x] 2.9 Update NZBget download path to /mnt/nas/mediastack/downloads/usenet (declarative)
- [ ] 2.10 Clean up old download dirs on nuc (/var/lib/deluge/complete, /var/lib/deluge/downloads, /nzb/dst)

## 3. Data migration (manual)

- [ ] 3.1 Migrate app state via rsync for each service
- [ ] 3.2 Configure download client paths in arr apps via web UI
- [ ] 3.3 Configure root folders in arr apps via web UI
- [ ] 3.4 Verify hardlink pipeline end-to-end
