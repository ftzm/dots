## 1. Storage

- [x] 1.1 Create narrow NFS PV for music /pool-1/mediastack/media/music (k8s)
- [x] 1.2 Create narrow NFS PV for audiobooks /pool-1/mediastack/media/audiobooks (k8s)
- [ ] 1.3 Create narrow NFS PV for cloud /pool-1/cloud (k8s) (deferred with Filestash)

## 2. Deploy

- [x] 2.1 Deploy Navidrome with music mount to navidrome namespace (k8s)
- [x] 2.2 Deploy Audiobookshelf with audiobooks mount to audiobookshelf namespace (k8s)
- [x] 2.3 Deploy The Lounge with config PVC to thelounge namespace (k8s)
- [ ] 2.4 Deploy Filestash with cloud mount to filestash namespace (k8s) (deferred)

## 3. Data migration (manual)

- [x] 3.1 Migrate Navidrome state via rsync (path fix: library → /music)
- [x] 3.2 Migrate Audiobookshelf state via rsync (path fix: all paths → /audiobooks)
- [x] 3.3 Migrate The Lounge state via rsync (replaced nix store symlink with real config.js)
- [x] 3.4 Verify all three services accessible (Filestash deferred)
