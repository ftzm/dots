# Migration Plan: Unify NixOS + k3s Infrastructure

## Context

Two parallel systems manage services on the nuc homelab: NixOS (dots repo, via comin) and k3s (cluster repo, via ArgoCD/Tanka). The goal is to consolidate toward k3s for application workloads, unify ingress under Traefik, eliminate duplicated infrastructure (observability, DNS), and clean up dead services. NixOS remains for host-level concerns and services that need hardware access.

---

## Target State

### Stays on NixOS (hardware/protocol reasons)
| Service | Why |
|---------|-----|
| Jellyfin | GPU transcoding (Intel QuickSync) |
| ~~Immich~~ | ~~GPU for ML inference~~ (moved to k8s — not using GPU) |
| MPD | Music daemon, host audio/network |
| Samba | Host-level SMB protocol |
| Mosquitto | MQTT broker, LAN IoT devices |
| Atuin + PostgreSQL | Tiny service, not worth migrating the database |
| Node exporter | Host metrics (scraped by k8s Prometheus) |
| nginx | WebDAV only (DAV verb handling) |

### Moves to k8s
| Service | Notes |
|---------|-------|
| Radarr | linuxserver.io image, PGID=1001 |
| Sonarr | linuxserver.io image, PGID=1001 |
| Lidarr | linuxserver.io image, PGID=1001 |
| Readarr | linuxserver.io image, PGID=1001 |
| Prowlarr | linuxserver.io image |
| Flaresolverr | CAPTCHA solver for Prowlarr |
| Deluge | linuxserver.io image, PGID=1001. Incoming peers via Traefik TCP entrypoint on port 6881 (router port-forward to nuc). |
| NZBget | linuxserver.io image, PGID=1001 |
| Navidrome | Official image |
| Audiobookshelf | Official image |
| Vaultwarden | Official image |
| ~~Atuin~~ | ~~Official image + bundled PostgreSQL StatefulSet~~ (stays on NixOS — not worth the PostgreSQL migration) |
| The Lounge | Official image |
| Filestash | Official image |
| Immich | Official Helm chart, bundles PostgreSQL (pgvecto.rs) + Redis |
| Jellyseerr | NEW, replaces Ombi |

### Remove entirely
| Service | Replacement |
|---------|-------------|
| Ombi | Jellyseerr (in k8s) |
| PhotoView + MySQL | Immich |
| Pigallery2 | Immich |
| Filebrowser | Immich / Filestash |
| Pi-hole (on pi) | Blocky (in k8s) |
| NixOS Loki | k8s Loki |
| NixOS Promtail | k8s Alloy (+ host journal scraping) |
| NixOS Grafana | k8s Grafana |
| NixOS Prometheus | Already disabled |
| Muscleup | No longer used |

---

## Namespaces

| Namespace | Apps | Rationale |
|-----------|------|-----------|
| `media` | Radarr, Sonarr, Lidarr, Readarr, Prowlarr, Flaresolverr, Deluge, NZBget, Jellyseerr | Tightly coupled — arr apps talk to download clients and each other. Same namespace = short service names. |
| `immich` | Immich server, ML, PostgreSQL, Redis | Helm chart manages its own multi-component deployment |
| `vaultwarden` | Vaultwarden | Independent |
| `navidrome` | Navidrome | Independent |
| `audiobookshelf` | Audiobookshelf | Independent |
| `thelounge` | The Lounge | Independent |
| `filestash` | Filestash | Independent |

## Domain & Access Pattern

Public services (accessible from internet) use `*.ftzmlab.xyz` on `web`/`websecure` entrypoints.
Private services (VPN only) use `*.lan.ftzmlab.xyz` on `privateweb`/`privatesecure` + `wgweb`/`wgsecure` entrypoints.

| Service | Domain | Access |
|---------|--------|--------|
| Jellyfin | `jellyfin.ftzmlab.xyz` | **Public** |
| Radarr | `radarr.lan.ftzmlab.xyz` | Private |
| Sonarr | `sonarr.lan.ftzmlab.xyz` | Private |
| Lidarr | `lidarr.lan.ftzmlab.xyz` | Private |
| Readarr | `readarr.lan.ftzmlab.xyz` | Private |
| Prowlarr | `prowlarr.lan.ftzmlab.xyz` | Private |
| Deluge | `deluge.lan.ftzmlab.xyz` | Private |
| Jellyseerr | `requests.lan.ftzmlab.xyz` | Private |
| Immich | `img.lan.ftzmlab.xyz` | Private |
| Vaultwarden | `vaultwarden.lan.ftzmlab.xyz` | Private |
| Navidrome | `navidrome.lan.ftzmlab.xyz` | Private |
| Audiobookshelf | `audiobookshelf.lan.ftzmlab.xyz` | Private |
| The Lounge | `irc.lan.ftzmlab.xyz` | Private |
| Filestash | `filestash.lan.ftzmlab.xyz` | Private |
| WebDAV | `dav.lan.ftzmlab.xyz` | Private |
| Grafana | `grafana.lan.ftzmlab.xyz` | Private (existing) |
| ArgoCD | `argo.lan.ftzmlab.xyz` | Private (existing) |
| ntfy | `ntfy.lan.ftzmlab.xyz` | Private (existing) |

NZBget, Flaresolverr, and Prowlarr don't strictly need ingress (accessed by other arr apps internally),
but Prowlarr has a web UI worth exposing.

---

## Implementation Approach: Jsonnet Helper for Self-Hosted Apps

Most services moving to k8s are simple single-container apps. Rather than vendoring Helm charts for each, define a reusable Jsonnet function in a new library file and stamp out each app in ~10 lines of config.

### New file: `lib/selfhosted.libsonnet`

```jsonnet
local k = import 'k8s-libsonnet/main.libsonnet';

{
  // Creates a standard self-hosted app: Deployment + Service + PVC + IngressRoute
  new(
    name,
    image,
    port,
    ns,
    // Optional overrides
    domain=null,           // e.g. 'radarr.lan.ftzmlab.xyz', null = no ingress
    puid=1000,
    pgid=1001,
    dataSize='1Gi',
    extraVolumes=[],       // list of {name, pv} for shared NFS mounts
    extraVolumeMounts=[],  // list of {name, mountPath}
    extraEnv={},           // additional env vars
    entryPoints=['privateweb', 'privatesecure'],
  ):: {
    local labels = { 'app.kubernetes.io/name': name },

    namespace: k.core.v1.namespace.new(ns),

    pvc: k.core.v1.persistentVolumeClaim.new(name + '-data')
      + k.core.v1.persistentVolumeClaim.metadata.withNamespace(ns)
      + k.core.v1.persistentVolumeClaim.spec.withAccessModes(['ReadWriteOnce'])
      + k.core.v1.persistentVolumeClaim.spec.resources.withRequests({ storage: configSize })
      + k.core.v1.persistentVolumeClaim.spec.withStorageClassName('nfs'),

    deployment: k.apps.v1.deployment.new(name, replicas=1, containers=[
      k.core.v1.container.new(name, image)
      + k.core.v1.container.withPorts([
        k.core.v1.containerPort.new(port),
      ])
      + k.core.v1.container.withEnvMap(
        { PUID: std.toString(puid), PGID: std.toString(pgid) } + extraEnv
      )
      + k.core.v1.container.withVolumeMounts(
        [k.core.v1.volumeMount.new('config', '/config')]
        + extraVolumeMounts
      ),
    ])
    + k.apps.v1.deployment.metadata.withNamespace(ns)
    + k.apps.v1.deployment.spec.selector.withMatchLabels(labels)
    + k.apps.v1.deployment.spec.template.metadata.withLabels(labels)
    + k.apps.v1.deployment.spec.template.spec.withVolumes(
      [k.core.v1.volume.fromPersistentVolumeClaim('config', name + '-data')]
      + extraVolumes
    ),

    service: k.core.v1.service.new(name, labels, [
      k.core.v1.servicePort.new(port, port),
    ])
    + k.core.v1.service.metadata.withNamespace(ns),

    [if domain != null then 'ingressRoute']: {
      apiVersion: 'traefik.io/v1alpha1',
      kind: 'IngressRoute',
      metadata: { name: name, namespace: ns },
      spec: {
        entryPoints: entryPoints,
        routes: [{
          match: 'Host(`' + domain + '`)',
          kind: 'Rule',
          services: [{ name: name, port: port }],
        }],
        tls: {},
      },
    },
  },
}
```

### Usage in main.jsonnet

```jsonnet
local selfhosted = import '../../lib/selfhosted.libsonnet';
local storage = import '../../lib/storage.libsonnet';

{
  // --- media namespace: arr stack + download clients ---
  media: {
    // Shared /data PV is defined once, all media apps reference it
    dataPV: storage.pv,
    dataPVC: storage.pvc,

    radarr: selfhosted.new(
      'radarr', 'lscr.io/linuxserver/radarr:5.16.3', 7878, 'media',
      domain='radarr.lan.ftzmlab.xyz',
      extraVolumes=[storage.dataVolume],
      extraVolumeMounts=[storage.dataMount],
    ),

    sonarr: selfhosted.new(
      'sonarr', 'lscr.io/linuxserver/sonarr:4.0.11', 8989, 'media',
      domain='sonarr.lan.ftzmlab.xyz',
      extraVolumes=[storage.dataVolume],
      extraVolumeMounts=[storage.dataMount],
    ),

    deluge: selfhosted.new(
      'deluge', 'lscr.io/linuxserver/deluge:2.1.1', 8112, 'media',
      domain='deluge.lan.ftzmlab.xyz',
      extraVolumes=[storage.dataVolume],
      extraVolumeMounts=[storage.dataMount],
    ),

    jellyseerr: selfhosted.new(
      'jellyseerr', 'fallenbagel/jellyseerr:2.3.0', 5055, 'media',
      domain='requests.lan.ftzmlab.xyz',
    ),

    // ... lidarr, readarr, prowlarr, nzbget, flaresolverr
  },

  // --- independent apps, each in own namespace ---
  vaultwarden: selfhosted.new(
    'vaultwarden', 'vaultwarden/server:1.32.7', 80, 'vaultwarden',
    domain='vaultwarden.lan.ftzmlab.xyz',
    puid=null, pgid=null,  // manages its own user
  ),

  navidrome: selfhosted.new(
    'navidrome', 'deluan/navidrome:0.53.3', 4533, 'navidrome',
    domain='navidrome.lan.ftzmlab.xyz',
    extraVolumes=[storage.dataVolume],
    extraVolumeMounts=[storage.dataMount],
  ),

  // ... audiobookshelf, thelounge, filestash
}
```

### Maintenance burden

- **Image updates**: one-line version bumps per app. Renovate can match container image tags via regex manager in `renovate.json`.
- **No chart versioning**: no `chartfile.yaml` entries, no vendored chart dirs, no values schema changes to track.
- **Helper changes**: if you need a new pattern (e.g. add a sidecar, change probe defaults), you change `selfhosted.libsonnet` once.
- **Escape hatch**: any app can override or extend the helper output with Jsonnet's `+:` operator for one-off needs.

### What stays as Helm charts
Keep Helm for complex multi-component deployments that would be painful to maintain by hand:
- kube-prometheus-stack (Prometheus, Grafana, Alertmanager, CRDs, RBAC)
- Traefik (CRDs, RBAC, complex config)
- cert-manager (CRDs, webhooks, RBAC)
- Loki, Tempo, Alloy (complex config)
- ArgoCD (multi-component)
- sealed-secrets, sops-operator

---

## Ingress: Single Traefik Entry Point

### Current problem
Three-layer proxy on some paths: nginx (WG) -> Traefik -> pod. Separate ACME certs on nginx vs cert-manager wildcard. Two different ingress configs to maintain.

### Target
Traefik is the sole ingress controller for all services. nginx remains only to handle WebDAV verbs, behind Traefik.

### Changes to cluster repo (main.jsonnet)
Add Traefik entrypoints for Wireguard and torrent peer traffic:
```
--entrypoints.wgweb.address=10.0.100.4:80
--entrypoints.wgsecure.address=10.0.100.4:443
--entrypoints.torrent.address=:6881
```

Add IngressRouteTCP for Deluge peer traffic:
```jsonnet
ingressRouteTCP: {
  apiVersion: 'traefik.io/v1alpha1',
  kind: 'IngressRouteTCP',
  metadata: { name: 'deluge-torrent', namespace: 'media' },
  spec: {
    entryPoints: ['torrent'],
    routes: [{
      match: 'HostSNI(`*`)',  // raw TCP, no SNI
      services: [{ name: 'deluge', port: 6881 }],
    }],
  },
},
```

Router port-forward: WAN:6881 → 192.168.1.4:6881 (nuc, where Traefik runs).
Deluge itself has no nodeSelector — Traefik routes to it via k8s Service.

### Changes to dots repo (nuc)
- Remove ALL nginx virtualHosts except the WebDAV backend (which Traefik proxies to)
- Remove nginx WG catch-all proxy
- Remove `security.acme` config (cert-manager wildcard covers everything)
- WebDAV: nginx listens on localhost only, Traefik routes `dav.ftzmlab.xyz` to it

### IngressRoutes for NixOS services
Jellyfin and WebDAV stay on NixOS but get Traefik ingress via k8s Services with static Endpoints pointing to the host IP (works because Traefik uses hostNetwork):

```jsonnet
// Example: Jellyfin (runs on NixOS, port 8096)
service: k.core.v1.service.new('jellyfin', { app: 'jellyfin' })
  + { spec+: { clusterIP: 'None' } },  // headless
endpoints: {
  apiVersion: 'v1', kind: 'Endpoints',
  metadata: { name: 'jellyfin' },
  subsets: [{ addresses: [{ ip: '192.168.1.4' }],
              ports: [{ port: 8096 }] }],
},
ingressRoute: { /* standard IngressRoute to this service */ },
```

---

## Storage & Permissions

### PV strategy: scoped mounts

Hardlinks require source and destination to be on the same filesystem mount. The download pipeline
(Deluge, NZBget, arr apps) needs hardlinks from downloads → media. A dedicated `mediastack`
directory scopes the mount to only what the pipeline needs — no access to `/pool-1/k8s`,
`/pool-1/cloud`, or other unrelated data.

Apps outside the pipeline (Navidrome, Audiobookshelf, Immich, Filestash) get narrow NFS PVs
pointing at only the subdirectory they need.

### Mediastack PV (broad, for download pipeline)

```
NFS mount: 192.168.1.3:/pool-1/mediastack → /data
  /data/downloads/torrents/complete
  /data/downloads/torrents/incomplete
  /data/downloads/usenet/complete
  /data/downloads/usenet/incomplete
  /data/media/movies
  /data/media/tv
  /data/media/music
  /data/media/books
  /data/media/audiobooks
```

Used by: Deluge, NZBget, Radarr, Sonarr, Lidarr, Readarr (all in `media` namespace).
Hardlinks work for both torrent and usenet imports — instant, zero I/O.

### Narrow PVs (scoped, for independent apps)

| App | NFS path | Mount | Access |
|-----|----------|-------|--------|
| Navidrome | `/pool-1/mediastack/media/music` | `/music` | read-only |
| Audiobookshelf | `/pool-1/mediastack/media/audiobooks` | `/audiobooks` | read-only |
| Immich | `/pool-1/media/photos` | `/photos` | read-write |
| Filestash | `/pool-1/cloud` | `/cloud` | read-write |

These are submounts of mediastack (or outside it entirely for Immich/Filestash).
No hardlink requirement, minimal blast radius.

### NAS directory creation & permissions (dots repo: `machines/nas/default.nix`)

Use `systemd.tmpfiles.rules` to declaratively ensure the directory tree exists with correct
ownership. This is the source of truth for the filesystem layout.

```nix
let
  storageGid = "storage"; # GID 1001, matches k8s PGID and NixOS storage group

  # The media pipeline directory layout — single source of truth
  mediastackDirs = [
    "/pool-1/mediastack"
    "/pool-1/mediastack/media"
    "/pool-1/mediastack/media/movies"
    "/pool-1/mediastack/media/tv"
    "/pool-1/mediastack/media/music"
    "/pool-1/mediastack/media/books"
    "/pool-1/mediastack/media/audiobooks"
    "/pool-1/mediastack/downloads"
    "/pool-1/mediastack/downloads/torrents"
    "/pool-1/mediastack/downloads/torrents/complete"
    "/pool-1/mediastack/downloads/torrents/incomplete"
    "/pool-1/mediastack/downloads/usenet"
    "/pool-1/mediastack/downloads/usenet/complete"
    "/pool-1/mediastack/downloads/usenet/incomplete"
  ];

  # Other managed directories
  otherDirs = [
    "/pool-1/media/photos"
    "/pool-1/cloud"
  ];

  # Download dirs get recursive permission enforcement (new files may arrive with wrong perms)
  downloadDirs = [
    "/pool-1/mediastack/downloads/torrents"
    "/pool-1/mediastack/downloads/usenet"
  ];
in {
  systemd.tmpfiles.rules =
    # d = create dir if missing, set ownership/mode. Safe, never deletes.
    map (dir: "d ${dir} 0775 root ${storageGid} -") (mediastackDirs ++ otherDirs)
    # Z = recursively fix ownership/mode on contents. Only on download dirs to avoid
    # walking the entire media library on every boot.
    ++ map (dir: "Z ${dir} 0775 root ${storageGid} -") downloadDirs;
}
```

### NAS NFS exports

The NAS already exports `/pool-1` as the root with `nohide`. Subdirectory mounts work without
additional exports. Verify the root export covers the new `mediastack` path.

### k8s storage config (cluster repo: `lib/storage.libsonnet`)

Encodes the pipeline as data. The `appAccess` map is the contract — read it top-to-bottom to verify
that every consumer's `reads` has a matching producer's `writes`.

```jsonnet
{
  nfs: {
    server: '192.168.1.3',
  },

  uid: 1000,
  gid: 1001,  // must match NAS storage group and NixOS storage group

  // Mediastack: download pipeline + media library (hardlink-capable single mount)
  mediastack: {
    nfsPath: '/pool-1/mediastack',
    mountPath: '/data',
    dirs: {
      media: {
        movies:     '/data/media/movies',
        tv:         '/data/media/tv',
        music:      '/data/media/music',
        books:      '/data/media/books',
        audiobooks: '/data/media/audiobooks',
      },
      downloads: {
        torrents:  { incomplete: '/data/downloads/torrents/incomplete',
                     complete:   '/data/downloads/torrents/complete' },
        usenet:    { incomplete: '/data/downloads/usenet/incomplete',
                     complete:   '/data/downloads/usenet/complete' },
      },
    },
  },

  // Narrow mounts for independent apps
  narrowMounts: {
    photos:     { nfsPath: '/pool-1/media/photos', mountPath: '/photos' },
    cloud:      { nfsPath: '/pool-1/cloud', mountPath: '/cloud' },
    music:      { nfsPath: '/pool-1/mediastack/media/music', mountPath: '/music' },
    audiobooks: { nfsPath: '/pool-1/mediastack/media/audiobooks', mountPath: '/audiobooks' },
  },

  // Pipeline contract: which app reads/writes which paths
  appAccess: {
    // Mediastack PV users (hardlink pipeline)
    deluge:         { mount: 'mediastack', writes: ['downloads.torrents'] },
    nzbget:         { mount: 'mediastack', writes: ['downloads.usenet'] },
    radarr:         { mount: 'mediastack', reads: ['downloads.torrents.complete', 'downloads.usenet.complete'],
                      writes: ['media.movies'] },
    sonarr:         { mount: 'mediastack', reads: ['downloads.torrents.complete', 'downloads.usenet.complete'],
                      writes: ['media.tv'] },
    lidarr:         { mount: 'mediastack', reads: ['downloads.torrents.complete', 'downloads.usenet.complete'],
                      writes: ['media.music'] },
    readarr:        { mount: 'mediastack', reads: ['downloads.torrents.complete', 'downloads.usenet.complete'],
                      writes: ['media.books'] },

    // Narrow PV users
    jellyseerr:     { mount: null },  // API only, no filesystem access
    immich:         { mount: 'photos', reads: ['photos'], writes: ['photos'] },
    navidrome:      { mount: 'music', reads: ['music'] },
    audiobookshelf: { mount: 'audiobooks', reads: ['audiobooks'] },
    filestash:      { mount: 'cloud', reads: ['cloud'], writes: ['cloud'] },

    // NixOS services (not in k8s, included for documentation)
    jellyfin:       { mount: 'nixos', reads: ['media.movies', 'media.tv', 'media.music'] },
  },

  // Static PV + PVC for the mediastack mount
  mediastackPV:: k.core.v1.persistentVolume.new('mediastack')
    + k.core.v1.persistentVolume.spec.withAccessModes(['ReadWriteMany'])
    + k.core.v1.persistentVolume.spec.withCapacity({ storage: '1Ti' })
    + k.core.v1.persistentVolume.spec.withPersistentVolumeReclaimPolicy('Retain')
    + k.core.v1.persistentVolume.spec.withStorageClassName('')  // no dynamic provisioning
    + { spec+: { nfs: { server: '192.168.1.3', path: '/pool-1/mediastack' } } },

  mediastackPVC:: k.core.v1.persistentVolumeClaim.new('mediastack')
    + k.core.v1.persistentVolumeClaim.metadata.withNamespace('media')
    + k.core.v1.persistentVolumeClaim.spec.withAccessModes(['ReadWriteMany'])
    + k.core.v1.persistentVolumeClaim.spec.withStorageClassName('')
    + k.core.v1.persistentVolumeClaim.spec.withVolumeName('mediastack')
    + k.core.v1.persistentVolumeClaim.spec.resources.withRequests({ storage: '1Ti' }),

  // Narrow PV+PVC helper for independent apps
  narrowPV(name, nfsPath, ns):: {
    pv: k.core.v1.persistentVolume.new(name)
      + k.core.v1.persistentVolume.spec.withAccessModes(['ReadWriteMany'])
      + k.core.v1.persistentVolume.spec.withCapacity({ storage: '1Ti' })
      + k.core.v1.persistentVolume.spec.withPersistentVolumeReclaimPolicy('Retain')
      + k.core.v1.persistentVolume.spec.withStorageClassName('')
      + { spec+: { nfs: { server: '192.168.1.3', path: nfsPath } } },
    pvc: k.core.v1.persistentVolumeClaim.new(name)
      + k.core.v1.persistentVolumeClaim.metadata.withNamespace(ns)
      + k.core.v1.persistentVolumeClaim.spec.withAccessModes(['ReadWriteMany'])
      + k.core.v1.persistentVolumeClaim.spec.withStorageClassName('')
      + k.core.v1.persistentVolumeClaim.spec.withVolumeName(name)
      + k.core.v1.persistentVolumeClaim.spec.resources.withRequests({ storage: '1Ti' }),
  },

  // Volume + mount helpers for use in selfhosted.new()
  mediastackVolume:: k.core.v1.volume.fromPersistentVolumeClaim('mediastack', 'mediastack'),
  mediastackMount:: k.core.v1.volumeMount.new('mediastack', '/data'),
}
```

### Per-app data (dynamic PVCs via nfs-provisioner)

Each app gets its own PVC for persistent state (databases, settings, metadata, etc.),
provisioned dynamically under `/pool-1/k8s`:
```
radarr-data, sonarr-data, lidarr-data, readarr-data,
prowlarr-data, deluge-data, nzbget-data, navidrome-data,
audiobookshelf-data, vaultwarden-data,
thelounge-data, filestash-data, jellyseerr-data
```

### Permissions alignment

| Layer | UID | GID | Mechanism |
|-------|-----|-----|-----------|
| NAS filesystem | root | 1001 (storage) | `systemd.tmpfiles.rules` |
| NFS export | any | any | `no_root_squash` |
| k8s pods (linuxserver.io) | 1000 | 1001 | `PUID`/`PGID` env vars |
| k8s pods (other) | 1000 | 1001 | `securityContext.fsGroup` |
| NixOS services (Jellyfin, etc.) | service user | 1001 (storage) | `group = "storage"` |

All layers agree on GID 1001. Files created by any layer are group-writable (0775).
Download dirs get recursive permission enforcement on NAS boot via `Z` tmpfiles rule.

### Download pipeline (end to end)

```
Deluge → /data/downloads/torrents/incomplete → /data/downloads/torrents/complete
NZBget → /data/downloads/usenet/incomplete  → /data/downloads/usenet/complete
Radarr → reads /data/downloads/*/complete   → hardlinks to /data/media/movies
Sonarr → reads /data/downloads/*/complete   → hardlinks to /data/media/tv
Lidarr → reads /data/downloads/*/complete   → hardlinks to /data/media/music
Readarr→ reads /data/downloads/*/complete   → hardlinks to /data/media/books
Jellyfin (NixOS) → reads /mnt/nas/mediastack/media (same NFS, mounted on host)
```

**Important:** Deluge currently downloads to local disk (`/var/lib/deluge`). Migration requires
changing download paths to the NFS share so arr pods on any node can access completed files.

**Hardlinks work** because downloads and media are subdirectories of the same NFS mount
(`/pool-1/mediastack`), so they're on the same filesystem. Both torrent and usenet imports
benefit — instant, zero-copy transfers.

---

## Secrets

Encrypted via SOPS (age) and committed to `environments/lab/secrets/`. The sops-secrets-operator
decrypts them in-cluster.

| App | Secrets needed |
|-----|---------------|
| Vaultwarden | `ADMIN_TOKEN`, env file (existing agenix secret, re-encrypt for SOPS) |
| Deluge | Auth file (existing agenix secret) |
| Immich | DB password, JWT secret (generated fresh on migration) |
| Atuin | N/A (stays on NixOS) |
| All others | None — arr apps, navidrome, etc. use internal auth configured via their web UIs |

---

## Data Migration Procedure

Copy existing NixOS app state into k8s PVCs before cutover. General approach per app:

1. Create the PVC in k8s (via `just render-lab` + ArgoCD sync)
2. Find the PVC's backing path on NAS: `ls /pool-1/k8s/` (nfs-provisioner creates subdirs)
3. Stop the NixOS service: `systemctl stop <service>`
4. rsync the data: `rsync -av /var/lib/<service>/ /mnt/nas/k8s/<pvc-subdir>/`
5. Deploy the k8s app, verify it starts with existing data
6. Remove the NixOS service from config

### App-specific notes

| App | Source path | Notes |
|-----|-----------|-------|
| Radarr | `/var/lib/radarr` | SQLite DB + config. Copy entire dir. |
| Sonarr | `/var/lib/sonarr/.config/Sonarr` | NixOS stores config under .config |
| Lidarr | `/var/lib/lidarr/.config/Lidarr` | Same pattern as Sonarr |
| Readarr | `/var/lib/readarr/.config/Readarr` | Same pattern |
| Prowlarr | `/var/lib/prowlarr/.config/Prowlarr` | Same pattern |
| Deluge | `/var/lib/deluge` | Config + state. Download dirs move to NFS separately. |
| NZBget | `/var/lib/nzbget` | Config + queue state |
| Vaultwarden | `/var/lib/vaultwarden` | **Critical** — contains encrypted password vault. Verify backup before migrating. |
| Navidrome | `/var/lib/navidrome` | SQLite DB with playlists, play counts, etc. |
| Audiobookshelf | `/var/lib/audiobookshelf` | Metadata DB + config |
| The Lounge | `/var/lib/thelounge` | User config, chat logs |
| Filestash | N/A | Stateless config, can start fresh |
| Immich | `/var/lib/immich` | PostgreSQL DB + thumbnails. May be easier to re-scan library than migrate DB. |
| Jellyseerr | N/A | New app, no existing data |

### Path remapping

After migration, arr apps need their download client and library paths updated in their web UIs:
- Root folder: `/media/movies` (was likely `/mnt/nas/...` or similar)
- Download client path: `/data/downloads/torrents/complete` (Deluge), `/data/downloads/usenet/complete` (NZBget)

---

## Backups

Back up only irreplaceable data. Extend the existing NAS Borg job:

```nix
services.borgbackup.jobs."borgbase" = {
  paths = [
    "/pool-1/media/photos"                  # was /pool-1/cloud/photos
    "/pool-1/k8s/vaultwarden-data-*"        # password vault — critical
  ];
  # ... rest unchanged
};
```

Everything else (arr databases, Navidrome playlists, etc.) is rebuildable and not worth the backup cost.

---

## Firewall

Intentionally disabled on nuc (`networking.firewall.enable = false`). No changes needed.

---

## Renovate: Container Image Tag Updates

All container images pinned to specific version tags (not `:latest`). Renovate creates PRs for updates.

Add a regex manager to `renovate.jsonnet` matching image strings in `main.jsonnet`:

```jsonnet
local containerManagers = [
  {
    customType: 'regex',
    managerFilePatterns: ['/environments/lab/main\\.jsonnet$/'],
    matchStrings: [
      "'(?<depName>[a-z0-9._/-]+):(?<currentValue>[a-z0-9._-]+)'",
    ],
    datasourceTemplate: 'docker',
  },
];

// merge into existing config:
customManagers: managers + containerManagers,
```

The existing `postUpgradeTasks` (`just render-lab`) re-renders manifests automatically.

---

## Migration Phases

### Phase 0: Cleanup
Remove from nuc NixOS config:
- PhotoView + photoview-db containers + create-photoview-network service
- Pigallery2 container
- Filebrowser container
- Ombi service
- Muscleup nginx virtualHost + static files
- NixOS Prometheus config (already disabled, remove the dead config)

### Phase 1: Observability Consolidation

**Host logs → k8s Loki:**
- **nuc** (k3s node): Add `hostPath` volume mount of `/var/log/journal` to the Alloy DaemonSet.
  Add `loki.source.journal` block to Alloy config. Covers all host services (Jellyfin, Mosquitto,
  nginx, systemd units, etc.).
- **nas** (not a k3s node): Keep Promtail as a NixOS service, repoint from localhost Loki to
  `https://loki.lan.ftzmlab.xyz/loki/api/v1/push`. Create a private-only IngressRoute for
  `loki.lan.ftzmlab.xyz` pointing at the Loki gateway service. No auth needed — private entrypoints
  only (Tailscale/WG).

**Host metrics → k8s Prometheus:**
- Add `additionalScrapeConfigs` to kube-prometheus-stack values:
  - `192.168.1.4:9002` (nuc node-exporter)
  - `192.168.1.3:9002` (nas node-exporter)

**Verify and clean up:**
- Verify host logs from both nuc and nas appear in k8s Grafana
- Verify node-exporter metrics from both hosts in k8s Prometheus
- Remove NixOS Loki, Promtail, Grafana from nuc config
- Remove NixOS Promtail config from nas (replace with repointed Promtail → k8s Loki)
- NixOS Prometheus on nuc already disabled, remove dead config

### Phase 2: Ingress Unification
- Add WG entrypoints to Traefik config (`wgweb`, `wgsecure` on 10.0.100.4)
- Create k8s Service + Endpoints for Jellyfin (static endpoint → 192.168.1.4:8096)
- Create IngressRoutes for all services (using wildcard cert)
- Jellyfin gets public entrypoints (`web`/`websecure`); everything else private only
- Set up WebDAV: nginx on localhost only + Traefik IngressRoute proxying to it
- Remove nginx virtualHosts and ACME config from NixOS
- Remove nginx WG catch-all

### Phase 3: Arr Stack + Download Clients
Migrate together as a group (they're tightly coupled):
- Create `mediastack` static NFS PV (`/pool-1/mediastack`, ReadWriteMany)
- Create `mediastack` directory tree on NAS (tmpfiles.rules)
- Deploy Deluge + NZBget with download paths under `/data/downloads/`
- Deploy Radarr, Sonarr, Lidarr, Readarr, Prowlarr, Flaresolverr
- Deploy Jellyseerr (new)
- Configure download clients in arr apps: `/data/downloads/torrents/complete` (Deluge), `/data/downloads/usenet/complete` (NZBget)
- Configure arr apps root folders: `/data/media/movies`, `/data/media/tv`, etc.
- Verify hardlink pipeline: download → import → file exists in both paths, single copy on disk
- Update Jellyfin library paths on NixOS: `/mnt/nas/mediastack/media/...`
- Remove all arr services + Deluge + NZBget + Ombi from NixOS config

### Phase 4: Remaining Services
Migrate one at a time, each is independent:
- **Immich**: Helm chart (`immich/immich`), bundles PostgreSQL (pgvecto.rs image) + Redis as subcharts. Mount `/data/media/photos`. Migrate existing library or re-scan. Vendor chart, add to `chartfile.yaml`.
- **Vaultwarden**: migrate config from `/var/lib/vaultwarden`, set up backup
- **Navidrome**: config PVC + `/data/media/music` mount
- **Audiobookshelf**: config PVC + `/data/media/audiobooks` mount
- **The Lounge**: config PVC only
- **Filestash**: config PVC + `/data/cloud` mount
- Remove each from NixOS config after verification

### Phase 5: DNS Consolidation
- Update Blocky `customDNS` mappings for all new `*.lan.ftzmlab.xyz` domains → `100.64.0.2` (Tailscale IP)
- Ensure `*.ftzmlab.xyz` (public, Jellyfin) resolves externally via Cloudflare DNS (ddclient or static)
- Update DHCP to point all LAN clients at Blocky
- Verify ad-blocking and custom DNS mappings
- Remove Pi-hole from pi config
- Evaluate if the pi machine is still needed (ddclient could move to k8s or another host)

---

## Repo Structure

### Current: two separate repos
- `dots` (NixOS configs for all machines + home-manager)
- `cluster` (k8s manifests, Tanka/Jsonnet)

### Target: monorepo
Merge cluster into dots. One repo for everything: NixOS configs, home-manager, and k8s cluster.

```
dots/
  machines/          # NixOS machine configs (existing)
  role/              # shared NixOS roles (existing)
  secrets/           # agenix secrets (existing)
  cluster/           # k8s cluster config (moved from separate repo)
    environments/lab/
    lib/
    charts/
    manifests/
    Justfile
    chartfile.yaml
    renovate.jsonnet
  flake.nix          # single flake for everything
```

### Benefits
- One commit can remove a NixOS service and add its k8s replacement
- Wireguard, secrets, MPD — all just files in the same repo, no coordination
- Comin URL stays the same for all machines
- One Renovate config, one git history
- Claude can see all infrastructure in one place

### Migration steps
- Move cluster repo contents into `dots/cluster/`
- Merge cluster's `flake.nix` dev shell into dots' `flake.nix`
- Update ArgoCD `Application` source path to point at the monorepo
- Update Renovate config paths
- Update `just` commands if needed (working directory change)
- Delete the standalone cluster repo

---

## Key Files to Modify

### monorepo (dots)
- `cluster/lib/storage.libsonnet` — NEW: storage topology, pipeline contract, NFS PV/PVC helpers
- `cluster/lib/selfhosted.libsonnet` — NEW: reusable helper for self-hosted app deployments
- `cluster/environments/lab/main.jsonnet` — add all new service definitions, Traefik entrypoints
- `cluster/lib/config.libsonnet` — add wireguard IP constant
- `cluster/environments/lab/secrets/` — encrypted secrets for Vaultwarden, Deluge auth, etc.
- `cluster/renovate.jsonnet` — add regex manager for container image tags in main.jsonnet
- `machines/nas/default.nix` — add `systemd.tmpfiles.rules` for media directory tree, update NFS exports
- `machines/nuc/default.nix` — remove migrated services, clean up nginx, remove ACME
- `machines/pi/default.nix` — remove Pi-hole, ddclient (if moved)
- `flake.nix` — merge cluster dev shell into existing flake

---

## Verification

After each phase:
- All IngressRoutes resolve and serve correctly over both Tailscale and Wireguard
- TLS works (wildcard cert) with no browser warnings
- For arr stack: end-to-end test of search -> download -> import -> available in Jellyfin
- Grafana shows host + k8s metrics and logs
- DNS resolution works for all `*.lan.ftzmlab.xyz` domains
- Vaultwarden accessible and data intact (critical — test before removing NixOS service)
