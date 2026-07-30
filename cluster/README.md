# Cluster Documentation

This repository defines a Kubernetes homelab cluster. All infrastructure is declared as code using
[Grafana Tanka](https://tanka.dev/) (Jsonnet), with Helm charts vendored into
the repo and rendered to static manifests that [ArgoCD](https://argo-cd.readthedocs.io/)
syncs to the cluster.

## Table of Contents

- [Development Environment](#development-environment)
- [Repository Layout](#repository-layout)
- [Toolchain Overview](#toolchain-overview)
- [Workflow: From Code to Cluster](#workflow-from-code-to-cluster)
- [Tanka and Jsonnet](#tanka-and-jsonnet)
- [Helm Chart Vendoring](#helm-chart-vendoring)
- [Justfile Commands](#justfile-commands)
- [Making Changes](#making-changes)
- [Networking and Routing](#networking-and-routing)
- [Secrets Management](#secrets-management)
- [Cluster Utilities](#cluster-utilities)
- [Observability Stack](#observability-stack)
- [Application Services](#application-services)
- [Dependency Automation (Renovate)](#dependency-automation-renovate)

---

## Development Environment

The repo is bootstrapped with [Nix Flakes](https://nixos.wiki/wiki/Flakes).
Running `nix develop` drops you into a shell with every tool needed.
No global installs are required; the flake pins nixpkgs to ensure reproducibility.

---

## Repository Layout

```
.
├── flake.nix                       # Nix dev shell definition
├── Justfile                        # Task runner commands
├── chartfile.yaml                  # Helm chart dependency declarations
├── jsonnetfile.json                # Jsonnet library dependencies (jb)
├── renovate.jsonnet                # Template that generates renovate.json
├── .sops.yaml                      # SOPS encryption rules (age key)
├── .gitleaks.toml                  # Secret-scanning config
│
├── lib/
│   └── config.libsonnet            # Shared constants (IPs, etc.)
│
├── environments/
│   └── lab/
│       ├── spec.json               # Tanka environment metadata
│       ├── main.jsonnet            # All cluster resources defined here
│       └── secrets/
│           └── *.enc.yaml          # SOPS-encrypted secrets
│
├── charts/                         # Vendored Helm charts (checked into git)
│   ├── argo-cd/
│   ├── traefik/
│   ├── kube-prometheus-stack/
│   └── ...
│
├── vendor/                         # Vendored Jsonnet libraries (jb)
│   └── github.com/
│       ├── grafana/jsonnet-libs/   # tanka-util (helm.template, etc.)
│       └── jsonnet-libs/           # k8s-libsonnet v1.34
│
├── manifests/
│   └── lab/                        # Rendered YAML manifests (ArgoCD reads these)
│
├── scripts/
│   └── create-sealed-secret.sh     # Helper for creating SealedSecrets
│
└── .github/workflows/
    └── renovate.yaml               # Nightly Renovate dependency updates
```

---

## Toolchain Overview

| Tool | Role |
|---|---|
| **Tanka** | Evaluates Jsonnet, templates Helm charts, exports Kubernetes YAML |
| **Jsonnet** | Data-templating language used to compose all resources |
| **Helm** | Charts are vendored and templated via Tanka's `helm.template()` — Helm is not used as a release manager |
| **ArgoCD** | Watches `manifests/lab/` in this repo and syncs to the cluster |
| **SOPS + age** | Encrypts secrets at rest in git; decrypted in-cluster by sops-secrets-operator |
| **Sealed Secrets** | Alternative secrets path using kubeseal + cluster-side controller |
| **Renovate** | Automated PRs for chart and dependency version bumps |
| **Just** | Task runner for rendering, diffing, and maintenance commands |

---

## Workflow: From Code to Cluster

```
 Edit main.jsonnet
        │
        ▼
 just render-lab          Tanka evaluates Jsonnet, templates Helm charts,
        │                 writes YAML to manifests/lab/
        ▼
 git commit & push        Rendered manifests committed to repo
        │
        ▼
 ArgoCD detects drift     Watches manifests/lab/ on HEAD
        │
        ▼
 Cluster state updated    selfHeal: true auto-syncs on drift
```

ArgoCD is configured with:
- **selfHeal: true** — automatically reconciles when cluster state drifts from git.
- **prune: false** — does not auto-delete resources removed from git (safety measure).
- **ServerSideApply: true** — uses server-side apply for conflict resolution.
- Source: `https://github.com/ftzm/cluster.git`, path `manifests/lab`.

---

## Tanka and Jsonnet

### Environment structure

There is a single Tanka environment: `environments/lab/`.

- **`spec.json`** — tells Tanka this is an environment (no hardcoded API server; uses current kubectl context).
- **`main.jsonnet`** — the single source of truth for the entire cluster. Every resource (Helm-templated or hand-written) is defined here.

### How it works

The top-level Jsonnet object is a map of logical groups (e.g. `traefik`, `monitoring`, `blocky`). Each group typically contains:

1. A `namespace` (created via `k.core.v1.namespace.new(...)`)
2. Helm-templated `resources` via `helm.template(name, chartPath, { values: ... })`
3. Custom Kubernetes objects written directly with `k8s-libsonnet`

A `withNamespace(resources, ns)` helper automatically adds namespace metadata to all namespaced resources from Helm output, while preserving explicit namespaces and skipping cluster-scoped kinds.

### Shared config

`lib/config.libsonnet` provides shared constants:

```jsonnet
{
  nasIP: '192.168.1.3',
  publicIP: '192.168.1.4',
  tailscaleIP: '100.64.0.2',
}
```

### Vendored Jsonnet libraries

Managed by `jsonnet-bundler` (`jb install` / `jb update`):

- **`k8s-libsonnet` v1.34** — typed Kubernetes object constructors (`k.core.v1.*`, `k.apps.v1.*`, etc.)
- **`tanka-util`** — `helm.template()` for rendering Helm charts inside Jsonnet

---

## Helm Chart Vendoring

Charts are declared in `chartfile.yaml` and vendored into `charts/` using
`tk tool charts vendor`. The full chart source is checked into git so that
charts can be inspected locally — useful for researching available chart values.
Custom logic and configuration is defined in Jsonnet, not in the charts themselves.

---

## Justfile Commands

| Command | Description |
|---|---|
| `just render-lab` | Delete old manifests, run `tk export` to regenerate YAML, copy encrypted secrets |
| `just render-all` | Alias for `render-lab` (scales to multiple environments) |
| `just diff-lab` | Show what would change if applied (`tk diff`) |
| `just jb-install` | Install vendored Jsonnet dependencies |
| `just jb-update` | Update Jsonnet dependencies to latest |
| `just generate-renovate` | Regenerate `renovate.json` from the Jsonnet template + validate |
| `just test-renovate` | Dry-run Renovate locally |

The render step produces flat YAML files named `{name}-{kind}.yaml` under
`manifests/lab/`, which is the directory ArgoCD watches.

---

## Making Changes

### Key concepts

`environments/lab/main.jsonnet` is the primary file where cluster resources are defined. Other files are involved depending on the change — `chartfile.yaml` for adding Helm charts, `lib/config.libsonnet` for shared constants, `environments/lab/secrets/` for encrypted secrets.

The top of `main.jsonnet` establishes the imports and helpers used throughout:

```jsonnet
local config = import '../../lib/config.libsonnet';
local helm = (import 'tanka-util/helm.libsonnet').new(std.thisFile);
local k = import 'k8s-libsonnet/main.libsonnet';
```

- `k` — typed Kubernetes object constructors (`k.core.v1.namespace.new(...)`, `k.apps.v1.deployment.new(...)`, etc.)
- `helm` — provides `helm.template()` for rendering vendored Helm charts into Jsonnet objects
- `config` — shared constants from `lib/config.libsonnet` (IP addresses)

The `withNamespace(resources, ns)` helper is defined at the top of the file. It adds namespace metadata to all namespaced resources from Helm output while skipping cluster-scoped kinds (ClusterRole, CRD, etc.) and preserving explicit namespace overrides.

### After every change

Every change to `main.jsonnet` requires rendering and committing:

1. Run `just render-lab` to regenerate the YAML manifests in `manifests/lab/`.
2. Commit **both** the Jsonnet source changes and the rendered manifests.
3. Push — ArgoCD will detect the new manifests and sync automatically.

Forgetting to render or forgetting to commit the rendered output will cause the cluster state to diverge from the Jsonnet source.

### Adding a Helm-based service

If the chart is not yet vendored, add it to `chartfile.yaml` and run `tk tool charts vendor`.

Then add a service block to `main.jsonnet`:

```jsonnet
myService: {
  local ns = 'my-service',

  namespace: k.core.v1.namespace.new(ns),

  resources: withNamespace(
    helm.template('my-service', '../../charts/my-chart', {
      namespace: ns,
      values: {
        // Helm values as a Jsonnet object
        replicaCount: 1,
        image: { repository: 'example/app', tag: 'latest' },
      },
    }),
    ns
  ),
},
```

### Adding a raw (non-Helm) service

For services defined entirely with `k8s-libsonnet`:

```jsonnet
myApp: {
  local ns = 'my-app',
  local labels = { app: 'my-app' },

  namespace: k.core.v1.namespace.new(ns),

  deployment: k.apps.v1.deployment.new('my-app')
    + k.apps.v1.deployment.metadata.withNamespace(ns)
    + k.apps.v1.deployment.spec.withReplicas(1)
    + k.apps.v1.deployment.spec.selector.withMatchLabels(labels)
    + k.apps.v1.deployment.spec.template.metadata.withLabels(labels)
    + k.apps.v1.deployment.spec.template.spec.withContainers([
      k.core.v1.container.new('my-app', 'image:tag')
      + k.core.v1.container.withPorts([
        k.core.v1.containerPort.newNamed(80, 'http'),
      ]),
    ]),

  service: k.core.v1.service.new('my-app', labels, [
    k.core.v1.servicePort.new(80, 80),
  ])
  + k.core.v1.service.metadata.withNamespace(ns),
},
```

### Adding an IngressRoute

Traefik IngressRoutes are defined as raw Jsonnet objects (not from a Helm chart). Add one inside a service block to expose it:

```jsonnet
ingressRoute: {
  apiVersion: 'traefik.io/v1alpha1',
  kind: 'IngressRoute',
  metadata: {
    name: 'my-app',
    namespace: ns,
  },
  spec: {
    entryPoints: ['privateweb', 'privatesecure'],
    routes: [{
      match: "Host(`my-app.lan.ftzmlab.xyz`)",
      kind: 'Rule',
      services: [{
        name: 'my-app',
        port: 80,
      }],
    }],
    tls: {},
  },
},
```

Use `entryPoints: ['privateweb', 'privatesecure']` for Tailscale-only access, or `['web', 'websecure']` for public access. The wildcard TLS cert covers all `*.lan.ftzmlab.xyz` hostnames automatically — just include `tls: {}`.

### Referencing secrets from services

Secrets are created via SOPS or Sealed Secrets (see [Secrets Management](#secrets-management)) and then referenced by name in service definitions.

**In Helm values** — use the chart's `existingSecret` pattern:

```jsonnet
values: {
  admin: {
    existingSecret: 'grafana-admin',
    userKey: 'admin-user',
    passwordKey: 'admin-password',
  },
},
```

**In raw k8s objects** — reference the secret by name in a volume mount, env var, or spec field:

```jsonnet
spec: {
  acme: {
    privateKeySecretRef: { name: 'letsencrypt-account-key' },
    solvers: [{
      dns01: {
        cloudflare: {
          apiTokenSecretRef: { name: 'cloudflare-api-token', key: 'api-token' },
        },
      },
    }],
  },
},
```

The secret name must match what the SOPS/SealedSecret resource creates in-cluster.

---

## Networking and Routing

### Dual-network architecture

Traefik runs with `hostNetwork: true` on the `nuc` node and binds to
separate IPs, creating isolated public and private ingress paths:

| Network | IP | Entrypoints | Ports | Use case |
|---|---|---|---|---|
| **Public (LAN)** | `192.168.1.4` | `web`, `websecure` | 80, 443 | Internet-routable services |
| **Private (Tailscale)** | `100.64.0.2` | `privateweb`, `privatesecure` | 80, 443 | Internal-only access via Tailscale VPN |
| **WireGuard** | `10.0.100.4` | `wgweb`, `wgsecure` | 80, 443 | WireGuard VPN access |

### How routing decisions work

- **Traefik IngressRoute CRD** is used for all services (not standard Ingress).
- Each IngressRoute specifies which `entryPoints` it listens on.
- Setting `entryPoints: ['privateweb', 'privatesecure']` makes a service reachable only over Tailscale.
- To expose a service publicly, add `web` and `websecure` to the entrypoints list.

### DNS

**Blocky** runs as a DNS proxy on all three network interfaces (`192.168.1.4:53`, `100.64.0.2:53`, `10.0.100.4:53`):

- Maps `lan.ftzmlab.xyz` → `100.64.0.2` (Tailscale IP) so all `*.lan.ftzmlab.xyz` subdomains resolve to the private Traefik entrypoints.
- Reachable from LAN, Tailscale, and WireGuard clients.
- Forwards `cluster.local` → `10.96.0.10` (CoreDNS) for in-cluster resolution.
- Provides ad-blocking via deny lists (StevenBlack, AdguardDNS, Firebog).

### TLS

- **cert-manager** obtains a wildcard certificate for `*.lan.ftzmlab.xyz` from Let's Encrypt using DNS-01 validation via the Cloudflare API.
- The wildcard cert is stored in the `traefik` namespace and set as the Traefik default TLS certificate via a `TLSStore` resource.
- All HTTPS IngressRoutes automatically use this wildcard cert.

### Service hostnames

The `lan` subdomain (`*.lan.ftzmlab.xyz`) is used to distinguish private services and enable DNS resolution for them over the Tailscale VPN.

---

## Secrets Management

Two complementary systems are available:

### SOPS + age

Used for secrets that need to be stored as encrypted files in git.

- `.sops.yaml` configures encryption rules: files matching `*.enc.yaml` have their `data` and `stringData` fields encrypted with an age public key.
- Encrypted files live in `environments/lab/secrets/` and are copied to `manifests/lab/` during rendering.
- The **sops-secrets-operator** runs in-cluster, mounts the age private key, and decrypts `SopsSecret` CRDs into regular Kubernetes Secrets.

### Sealed Secrets

Used for secrets created interactively.

- `scripts/create-sealed-secret.sh` takes key-value pairs, creates a dry-run Secret, encrypts it with `kubeseal`, and outputs Jsonnet-ready `SealedSecret` resources.
- The **sealed-secrets controller** runs in-cluster and decrypts `SealedSecret` CRDs.

### Secret scanning

`gitleaks` is configured (`.gitleaks.toml`) to detect accidentally committed secrets, with allowlists for chart test fixtures. It should always be run before making any changes.

---

## Cluster Utilities

### Storage — NFS Subdir External Provisioner

- Connects to a NAS at `192.168.1.3:/pool-1/k8s`.
- Creates a default `StorageClass` named `nfs`.
- All PersistentVolumeClaims in the cluster (Prometheus, Loki, Tempo, Grafana, ntfy) are dynamically provisioned here.

### Ingress — Traefik v3

- Dual-network reverse proxy (see [Networking and Routing](#networking-and-routing)).
- Exports JSON access logs (collected by Alloy → Loki).
- Prometheus metrics on port 9091 (avoids conflict with node-exporter on 9100).
- Monitored via a `PodMonitor`.

### GitOps — ArgoCD

- Watches `manifests/lab/` in this repo.
- Auto-heals drift; does not auto-prune (safe deletion requires manual action).
- Accessible at `argo.lan.ftzmlab.xyz` via TLS passthrough.

### Certificate Automation — cert-manager

- `ClusterIssuer` named `letsencrypt` using ACME DNS-01 with Cloudflare.
- Wildcard `Certificate` for `*.lan.ftzmlab.xyz` stored in the `traefik` namespace.

---

## Observability Stack

All observability components live in the `monitoring` namespace.

### Prometheus (via kube-prometheus-stack)

- 20Gi NFS-backed storage, 30-day retention (18GB size limit).
- Discovers all `ServiceMonitor` and `PodMonitor` resources cluster-wide (not restricted by Helm release labels).
- Disabled components not relevant to homelab: etcd, kube-controller-manager, kube-scheduler, kube-proxy.

### Grafana

- Admin credentials via SealedSecret.
- Persistent storage (1Gi NFS).
- Pre-configured datasources: Prometheus (built-in), Loki, Tempo.
- Dashboard auto-discovery from all namespaces.
- Accessible at `grafana.lan.ftzmlab.xyz`.

### Alertmanager

- Routes all alerts to **ntfy** (self-hosted push notifications) via webhook.
- Watchdog alerts silenced.
- Inhibition rules suppress lower-severity alerts when higher-severity ones fire.
- Disabled alerts (via `defaultRules.disabled` in Helm values):
  - **KubeMemoryOvercommit** — cluster is too small to tolerate node failure.
  - **NodeNetworkTransmitErrs** — false positive from Tailscale's WireGuard interface (`wg0`), which doesn't populate standard Linux network error counters.

### Loki — Log Aggregation

- Runs in SingleBinary (monolithic) mode.
- Filesystem-backed TSDB storage, 20Gi NFS volume.
- 30-day retention with compactor-based cleanup.
- Gateway enabled for API access.

### Tempo — Distributed Tracing

- Accepts traces via OTLP (gRPC on 4317, HTTP on 4318).
- 14-day retention, 10Gi NFS volume.

### Alloy — Unified Collector

- Deployed as a DaemonSet on every node.
- **Log collection:** Discovers all pods via the Kubernetes API, collects container logs, parses JSON fields (level, msg), normalizes log levels, and pushes to Loki.
- **Trace forwarding:** Runs an OTLP receiver (gRPC + HTTP), batches traces, and exports to Tempo.
- Relabeling extracts `namespace`, `pod`, `container`, `node`, and `app` labels.

---

## Application Services

### ntfy — Push Notifications

- Self-hosted notification server (`binwiederhier/ntfy`).
- Receives webhook alerts from Alertmanager.
- 1Gi NFS-backed cache with 12h history.
- Accessible at `ntfy.lan.ftzmlab.xyz`.

### Miniflux — Feed Reader

- `miniflux/miniflux`, backed by its own CloudNativePG cluster (`miniflux-database`).
  No superuser and no extensions required; it connects as the database owner via
  the operator-generated `miniflux-database-app` secret.
- Read/unread and starred state lives server-side and is exposed over the Google
  Reader and Fever APIs, so native clients on every device stay in sync. Auth is
  Miniflux's own (the mobile apps hit the API directly), same as PinePods.
- Admin password comes from the `miniflux-admin` SopsSecret; `CREATE_ADMIN` is
  idempotent, so it stays enabled.
- `/metrics` is exported to Prometheus via a ServiceMonitor (feed polling fails
  quietly, so the collector is the only signal that a feed has died).
- Daily `pg_dump` to `/pool-1/k8s/miniflux-db-backup`, which the NAS borg job covers.
- Accessible at `miniflux.lan.ftzmlab.xyz`.

### Hello World — Test App

- Minimal nginx deployment used to verify ingress and DNS are working.
- Private-only IngressRoute at `hello.lan.ftzmlab.xyz`.

### Storage Test

- A busybox pod that mounts an NFS PVC and writes a timestamp, verifying that dynamic provisioning works.

---

## PostgreSQL Images and Major Versions

Three CloudNativePG clusters run here, all single-instance on NFS:

| Cluster | Namespace | Catalog | Major |
|---|---|---|---|
| `immich-database` | `immich` | `vectorchord` | 16 |
| `miniflux-database` | `miniflux` | `postgresql` | 18 |
| `pinepods-database` | `pinepods` | `postgresql` | 18 |

None of them set `imageName` directly. They reference a cluster-scoped
`ClusterImageCatalog` by major, built by `lib/postgres.libsonnet`.

The reason is that a bare image tag makes two very different things look the
same:

- **patch within a major** — routine, and safe to take unattended;
- **change of major** — the operator shuts the whole cluster down and runs
  `pg_upgrade --link`, destroying and re-cloning replica PVCs. With
  `instances: 1` there is no replica to absorb it, so it is hard downtime.

With a catalog, moving majors is an explicit edit of `major:` at the Cluster,
and can never happen as a side effect of a version bump. Each catalog lists
only the majors actually in use, so an upgrade means adding the new image to
the catalog *and* changing the Cluster's `major` — two deliberate edits.

`majorOf()` derives each catalog entry's major from its own tag, so the
declared major and the image cannot drift. It handles vectorchord's coupled
`<postgres>-<extension>` form too (`16.9-0.4.3` → `16`). If a bump ever moved
an image across majors, the catalog would stop offering the major a Cluster
asks for; the operator reports an error and leaves the running database alone
rather than quietly migrating it.

### Before changing a major

CloudNativePG will do the upgrade, but it explicitly does **not** handle
extensions: *"CloudNativePG is not responsible for PostgreSQL extensions. You
must ensure that extensions in the source PostgreSQL image are compatible with
those in the target image."* Things to settle first:

- Take a fresh dump. The nightly `pgDumpCronJob` (`lib/backup.libsonnet`) is
  the restore path — `pg_upgrade --link` hard-links the old data directory, so
  once the new server starts the pre-upgrade copy is *not* a safe fallback.
  Rollback by reverting the image only works while the upgrade job is failing.
- Avoid PostgreSQL 17.0–17.5 as a target: a known bug blocks upgrades unless
  `max_slot_wal_keep_size = -1`. Go to 17.6+ instead.
- Source and target images must share an OS distribution base.
- Afterwards, run the `update_extensions.sql` that `pg_upgrade` emits, then
  `ANALYZE` — `pg_upgrade` does not carry optimizer statistics across.

---

## Dependency Automation (Renovate)

Renovate runs daily at 04:00 UTC via GitHub Actions (`.github/workflows/renovate.yaml`).

### How it works

1. `renovate.jsonnet` reads `chartfile.yaml` and generates a `renovate.json` config with a custom regex manager per Helm chart.
2. Renovate detects version bumps in chart registries.
3. When a new version is found, it creates a PR that:
   - Updates the version in `chartfile.yaml`.
   - Runs post-upgrade tasks: `tk tool charts vendor --prune` and `just render-lab`.
   - Commits the updated chart source and re-rendered manifests.
4. The GitHub Actions workflow installs Nix to get all required tools.

### Automerge

Minor, patch, and digest updates automerge once the required `build` check
passes. This mirrors the `flake.lock` posture in
`.github/workflows/update-flake-lock.yml`, which has merged nixpkgs bumps
unattended since PR #65 — these updates are strictly narrower than that.

`platformAutomerge: true` hands the merge to GitHub rather than Renovate, so a
PR that goes green at 04:10 merges immediately instead of waiting ~24h for the
next Renovate run. It requires at least one required status check on `master`,
which `build` satisfies.

Major updates deliberately do **not** automerge.

#### vectorchord: a coupled tag

`ghcr.io/tensorchord/cloudnative-vectorchord` tags are
`<postgres-version>-<vchord-version>` (e.g. `16.9-0.4.3`). Renovate's default
docker versioning reads everything after the `-` as an immutable compatibility
suffix, exactly as it would for `-alpine`. That had two invisible effects:

- it only ever offered tags carrying our exact `-0.4.3`, so the newest
  candidate was `17.5-0.4.3` — a PostgreSQL **major** jump, to a stale patch
  level at that (the PG 17 line is on 17.10); and
- it could never offer a vchord upgrade at all, because every extension bump
  changes the suffix. We sat on 0.4.3 while upstream shipped 1.1.1, and no PR
  would ever have said so.

A `versioning: regex:` rule parses both halves, so the PG major lands in
`major` and extension updates become visible. Renovate now proposes
`16.14-1.1.1` and `18.4-1.1.1` instead of `17.5-0.4.3`.

The image also sets `automerge: false` unconditionally. A CNPG `imageName`
change across PG majors triggers an offline in-place `pg_upgrade`, and the
`16.14-1.1.1` candidate — typed *minor*, since PG only moves 16.9→16.14 —
still carries a vchord major (0.4→1.1) needing its own `ALTER EXTENSION`.
Neither is judgeable from the tag, so this image always goes through review.

### Triage of what doesn't automerge

`.github/workflows/renovate-triage.yml` picks up every Renovate PR that the
automerge rule leaves behind. It runs on CI completion for `renovate/*`
branches, and on `workflow_dispatch` (optionally with a single PR number) to
work through a backlog.

| Situation | Mode | What happens |
|---|---|---|
| CI failed | `fix` | Claude diagnoses the breakage and opens a repair PR **against the Renovate branch** — not `master` — so merging it lets the original PR go green and automerge. Renovate force-pushes its own branches on rebase, which is why the fix is not committed directly onto it. |
| CI passed, won't automerge | `review` | Claude fetches the release notes for the full version span, works out how this repo actually uses the component, and either merges it or posts structured investigation notes and applies the `needs-review` label. |

The review path never merges a data-bearing component (CloudNativePG,
vectorchord, valkey with persistence, forgejo, immich, audiobookshelf,
navidrome) regardless of how clean the notes look — a major bump there can
force an on-disk migration that CI cannot see and `git revert` cannot undo.

Note that container-image PRs frequently arrive with **no** release notes in
the PR body (`Some dependencies could not be looked up`), so the reviewer is
told to go fetch them from upstream rather than trust the body.

### Managed dependency types

- Helm chart versions (`chartfile.yaml`)
- GitHub Actions versions (`.github/workflows/`)
- Jsonnet bundler dependencies (`jsonnetfile.json`)
