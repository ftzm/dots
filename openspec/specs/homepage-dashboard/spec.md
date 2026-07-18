# homepage-dashboard Specification

## Purpose
Deploy the Homepage (gethomepage.dev) dashboard to the cluster as a private landing page listing all self-hosted services with health indicators.

## Requirements

### Requirement: Homepage deployment in dedicated namespace
The system SHALL deploy Homepage (gethomepage.dev) into a `homepage` namespace as a Deployment with a Service on port 3000, using the vendored Helm chart rendered via Tanka.

#### Scenario: Homepage pod is running
- **WHEN** ArgoCD syncs the homepage namespace
- **THEN** a Homepage pod is running and healthy on port 3000

### Requirement: Homepage accessible via private IngressRoute
The system SHALL expose Homepage at `home.lan.ftzmlab.xyz` via a Traefik IngressRoute on private and WireGuard entrypoints (privateweb, privatesecure, wgweb, wgsecure) with TLS.

#### Scenario: Dashboard loads over HTTPS
- **WHEN** a user navigates to `https://home.lan.ftzmlab.xyz` from a WireGuard/Tailscale client
- **THEN** the Homepage dashboard loads successfully

### Requirement: Service links grouped by category
The system SHALL display service links organized into groups: Media, Apps, and Infrastructure. Each service entry SHALL include a name, icon, and href pointing to its `*.lan.ftzmlab.xyz` domain.

#### Scenario: All services are listed
- **WHEN** the dashboard loads
- **THEN** all services (Radarr, Sonarr, Lidarr, Readarr, Prowlarr, Jellyseerr, Jellyfin, Immich, Navidrome, Audiobookshelf, Vaultwarden, The Lounge, ntfy, ArgoCD, Grafana) are visible in their respective groups

### Requirement: HTTP health ping per service
The system SHALL perform HTTP ping checks against each service's URL and display a green/red status indicator on the dashboard tile.

#### Scenario: Healthy service shows green
- **WHEN** a service responds with HTTP 2xx
- **THEN** its tile displays a green status indicator

#### Scenario: Unreachable service shows red
- **WHEN** a service does not respond or returns non-2xx
- **THEN** its tile displays a red status indicator

### Requirement: Configuration via ConfigMap
The system SHALL store all Homepage configuration (services.yaml, widgets.yaml, settings.yaml) in a ConfigMap mounted into the Homepage pod. No persistent storage is required.

#### Scenario: Config changes deploy via git
- **WHEN** the ConfigMap content changes in the Jsonnet source
- **THEN** ArgoCD syncs the updated ConfigMap and Homepage picks up the new configuration

### Requirement: Dark theme with clean layout
The system SHALL use a dark theme with row-based layout for service groups.

#### Scenario: Dashboard renders in dark mode
- **WHEN** the dashboard loads
- **THEN** it uses a dark color scheme with services displayed in a row/grid layout per group
