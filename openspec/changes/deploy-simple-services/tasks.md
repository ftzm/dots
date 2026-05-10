## 1. Storage

- [ ] 1.1 Create narrow NFS PV for music /pool-1/mediastack/media/music (k8s)
- [ ] 1.2 Create narrow NFS PV for audiobooks /pool-1/mediastack/media/audiobooks (k8s)
- [ ] 1.3 Create narrow NFS PV for cloud /pool-1/cloud (k8s)

## 2. Deploy

- [ ] 2.1 Deploy Navidrome with music mount to navidrome namespace (k8s)
- [ ] 2.2 Deploy Audiobookshelf with audiobooks mount to audiobookshelf namespace (k8s)
- [ ] 2.3 Deploy The Lounge with config PVC to thelounge namespace (k8s)
- [ ] 2.4 Deploy Filestash with cloud mount to filestash namespace (k8s)

## 3. Data migration (manual)

- [ ] 3.1 Migrate Navidrome state via rsync
- [ ] 3.2 Migrate Audiobookshelf state via rsync
- [ ] 3.3 Migrate The Lounge state via rsync
- [ ] 3.4 Verify all four services accessible
