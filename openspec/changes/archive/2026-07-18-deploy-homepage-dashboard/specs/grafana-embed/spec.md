## ADDED Requirements

### Requirement: Grafana anonymous read-only access
The system SHALL enable anonymous access on Grafana with Viewer role, allowing unauthenticated users on the private network to view dashboards without login.

#### Scenario: Unauthenticated iframe loads dashboard
- **WHEN** Homepage renders an iframe pointing to Grafana
- **THEN** the Grafana dashboard content displays without a login prompt

### Requirement: Grafana allows embedding
The system SHALL set `security.allow_embedding = true` and `cookie_samesite = lax` in Grafana's configuration to permit iframe embedding from other origins on the same network.

#### Scenario: Iframe is not blocked by X-Frame-Options
- **WHEN** Homepage embeds a Grafana URL in an iframe
- **THEN** the browser does not block the iframe due to X-Frame-Options or CSP headers

### Requirement: Dedicated cluster overview Grafana dashboard
The system SHALL provision a Grafana dashboard via ConfigMap (auto-discovered by the sidecar) containing panels for: node CPU usage, node memory usage, pod health summary (running vs not-ready), and NFS disk usage.

#### Scenario: Dashboard exists in Grafana
- **WHEN** the monitoring namespace ConfigMap is synced
- **THEN** a "Homepage Overview" dashboard appears in Grafana with node CPU, memory, pod health, and disk panels

### Requirement: Homepage embeds Grafana overview as iframe
The system SHALL include an iframe widget in Homepage's widgets.yaml pointing to the cluster overview dashboard in kiosk/solo mode.

#### Scenario: Cluster metrics visible on homepage
- **WHEN** the homepage loads
- **THEN** the top section displays an embedded Grafana dashboard showing cluster health metrics
