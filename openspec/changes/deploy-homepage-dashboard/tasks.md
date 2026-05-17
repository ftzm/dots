## 1. Vendor Homepage Helm Chart

- [x] 1.1 Add Homepage Helm chart repository to `cluster/chartfile.yaml`
- [x] 1.2 Add Homepage chart version requirement to `cluster/chartfile.yaml`
- [x] 1.3 Run chart vendoring to pull chart into `cluster/charts/homepage/`

## 2. Grafana Configuration for Embedding

- [x] 2.1 Add `grafana.ini` settings to kube-prometheus-stack values in `main.jsonnet`: enable anonymous access (Viewer role), allow_embedding, cookie_samesite lax
- [x] 2.2 Create a Grafana dashboard JSON (ConfigMap in monitoring namespace with sidecar label) containing node CPU, node memory, pod health, and NFS disk usage panels
- [x] 2.3 Render manifests and verify Grafana config changes apply cleanly

## 3. Homepage Deployment in main.jsonnet

- [x] 3.1 Add `homepage` top-level key in `main.jsonnet` with namespace, Helm template rendering, Service, and IngressRoute at `home.lan.ftzmlab.xyz`
- [x] 3.2 Define the ConfigMap with `settings.yaml` (dark theme, row layout, title)
- [x] 3.3 Define the ConfigMap with `services.yaml` (Media, Apps, Infrastructure groups with all service links, icons, and ping URLs)
- [x] 3.4 Define the ConfigMap with `widgets.yaml` (Grafana iframe widget + kubernetes cluster/node widget)
- [x] 3.5 Render manifests and verify no errors

## 4. App Widget API Keys

- [x] 4.1 Retrieve API keys from running services (Radarr, Sonarr, Lidarr, Readarr, Prowlarr, Immich)
- [x] 4.2 Create age-encrypted SopsSecret in homepage namespace containing all API keys
- [x] 4.3 Configure Homepage deployment to mount SopsSecret as env vars with HOMEPAGE_VAR_ prefix
- [x] 4.4 Add widget configurations to services.yaml entries using cluster-internal service URLs and HOMEPAGE_VAR_ key references

## 5. Deploy and Verify

- [x] 5.1 Render full manifests with `tk show` / `just render-lab`
- [ ] 5.2 Commit and push — let ArgoCD sync
- [ ] 5.3 Verify `home.lan.ftzmlab.xyz` loads with dark theme and service groups
- [ ] 5.4 Verify Grafana iframe renders cluster overview without login prompt
- [ ] 5.5 Verify health ping dots show correct status for running services
- [ ] 5.6 Verify app widgets display data (arr queues, Immich stats)
