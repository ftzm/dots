## Context

The homelab runs ~16 services on Kubernetes (Traefik IngressRoutes, ArgoCD GitOps, kube-prometheus-stack for monitoring). Services are accessed via `*.lan.ftzmlab.xyz` domains on private/WireGuard entrypoints. There is no unified view — users navigate by memory or bookmarks. Grafana exists at `grafana.lan.ftzmlab.xyz` but requires authentication and is metrics-focused, not a service launcher.

All cluster config is Jsonnet (Tanka) rendered to manifests and synced by ArgoCD. Services follow patterns in `selfhosted.libsonnet`. Helm charts are vendored via `chartfile.yaml`.

## Goals / Non-Goals

**Goals:**
- Single-page dashboard at `home.lan.ftzmlab.xyz` with service links, health status, and cluster overview
- Embedded Grafana iframe showing node/pod/disk health at a glance
- App-specific widgets (arr queues, Immich stats) for richer status
- Entirely declarative — config in Jsonnet, secrets in SopsSecret, deployed via ArgoCD

**Non-Goals:**
- Replacing Grafana for detailed monitoring/alerting
- Auto-discovery of services (manual definition in Jsonnet is preferred for visibility)
- Public access — dashboard is private/WireGuard only
- Complex RBAC or multi-user access on Homepage

## Decisions

### 1. Homepage over Dashy/Homer/Homarr

**Choice**: Homepage (gethomepage.dev)

**Rationale**: YAML-config-driven (fits ConfigMap pattern), official Helm chart, native app widgets for arr/Immich without custom integration, built-in HTTP ping health checks, iframe widget for Grafana embedding. Dashy is close but lacks the native widget ecosystem.

### 2. Grafana anonymous read-only for embedding

**Choice**: Enable `auth.anonymous` with `org_role: Viewer` and `allow_embedding: true`

**Rationale**: Homepage iframe cannot pass authentication headers. Since Grafana is only exposed on private/WireGuard networks, anonymous viewer access is acceptable. Alternative (auth proxy) adds complexity with no security benefit on a private network.

### 3. Dedicated provisioned Grafana dashboard for embed

**Choice**: Create a ConfigMap-provisioned dashboard with specific panels, embed as single iframe

**Rationale**: Embedding individual panels requires managing multiple iframe URLs. A single purpose-built dashboard keeps the Homepage config simple (one URL) and allows iterating on which panels to show by editing the Grafana dashboard JSON without touching Homepage config. The sidecar auto-discovers dashboards from any namespace.

### 4. API keys via SopsSecret + HOMEPAGE_VAR_ env vars

**Choice**: Store API keys in age-encrypted SopsSecret, expose as env vars with `HOMEPAGE_VAR_` prefix

**Rationale**: Homepage's native secret mechanism uses `{{HOMEPAGE_VAR_X}}` placeholders in YAML resolved from environment variables. SopsSecret is the established secrets pattern in this cluster. Alternative (sealed secrets) works too but SopsSecret is simpler for multi-key secrets.

### 5. Helm chart via chartfile.yaml vendoring

**Choice**: Vendor the Homepage Helm chart like all other charts

**Rationale**: Consistent with existing pattern (nfs-provisioner, traefik, kube-prometheus-stack, etc.). Tanka's `helm.template()` renders it inline in main.jsonnet.

## Risks / Trade-offs

- **Grafana anonymous access** → Any device on the private network can view dashboards without login. Mitigated by network being WireGuard/Tailscale only (trusted devices).
- **API key rotation** → If arr app keys change, SopsSecret must be re-encrypted and committed. Low risk — these keys rarely change.
- **Homepage chart updates** → Vendored charts require manual version bumps. Same trade-off as all other vendored charts.
- **Iframe cross-origin** → Both Homepage and Grafana are on `*.lan.ftzmlab.xyz` with the same wildcard cert. Should work without CORS issues since they share the domain suffix. If issues arise, Grafana's `cookie_samesite: lax` setting handles it.
