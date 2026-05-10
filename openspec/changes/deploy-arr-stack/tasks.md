## 1. Storage

- [ ] 1.1 Create mediastack static NFS PV and PVC in main.jsonnet (k8s)

## 2. Deploy apps

- [ ] 2.1 Deploy Deluge with mediastack volume (k8s)
- [ ] 2.2 Deploy NZBget with mediastack volume (k8s)
- [ ] 2.3 Deploy Radarr with mediastack volume (k8s)
- [ ] 2.4 Deploy Sonarr with mediastack volume (k8s)
- [ ] 2.5 Deploy Lidarr with mediastack volume (k8s)
- [ ] 2.6 Deploy Readarr with mediastack volume (k8s)
- [ ] 2.7 Deploy Prowlarr (k8s)
- [ ] 2.8 Deploy Flaresolverr (k8s)
- [ ] 2.9 Deploy Jellyseerr (k8s)
- [ ] 2.10 Add IngressRouteTCP for Deluge peer traffic on port 6881 (k8s)

## 3. Data migration (manual)

- [ ] 3.1 Migrate app state via rsync for each service
- [ ] 3.2 Configure download client paths in arr apps via web UI
- [ ] 3.3 Configure root folders in arr apps via web UI
- [ ] 3.4 Verify hardlink pipeline end-to-end
