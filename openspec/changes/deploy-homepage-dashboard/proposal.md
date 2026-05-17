## Why

There is no unified dashboard for the homelab. Services are accessed by remembering URLs or bookmarks. A single page is needed that provides quick-launch links to all services, at-a-glance cluster health via embedded Grafana panels, and per-service status indicators — all managed declaratively through git.

## What Changes

- Deploy Homepage (gethomepage.dev) into a new `homepage` namespace via Helm chart
- Configure service links grouped by category (Media, Apps, Infrastructure) with HTTP health pings
- Embed a Grafana cluster overview dashboard (node CPU/memory, pod health, disk usage) via iframe
- Enable anonymous read-only access on Grafana to allow iframe embedding
- Set up app-specific widgets (Radarr queue, Sonarr queue, Immich stats, etc.) with API keys stored in SopsSecret
- Create a dedicated Grafana dashboard provisioned via ConfigMap for the homepage embed

## Capabilities

### New Capabilities
- `homepage-dashboard`: Homepage deployment, service links, health pings, layout configuration, and IngressRoute
- `grafana-embed`: Grafana anonymous access, embedding configuration, and a purpose-built overview dashboard for the homepage iframe
- `homepage-widgets`: App-specific widget integration with API key management via SopsSecret

### Modified Capabilities

## Impact

- **New namespace**: `homepage`
- **New Helm chart dependency**: Homepage chart vendored via chartfile.yaml
- **Grafana config change**: Enable anonymous read-only access and allow_embedding in kube-prometheus-stack values
- **New secrets**: SopsSecret with API keys for arr apps and Immich
- **New IngressRoute**: `home.lan.ftzmlab.xyz` on private+wg entrypoints
- **New Grafana dashboard**: ConfigMap-provisioned dashboard in monitoring namespace
