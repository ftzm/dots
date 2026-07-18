# homepage-widgets Specification

## Purpose
Configure app-specific Homepage widgets that surface live data (queues, stats) from services via their APIs, with API keys managed as encrypted secrets.

## Requirements

### Requirement: App-specific widgets for arr services
The system SHALL configure Homepage widgets for Radarr, Sonarr, Lidarr, Readarr, and Prowlarr that display queue/activity information using each service's API.

#### Scenario: Radarr widget shows queue count
- **WHEN** the dashboard loads and Radarr is reachable
- **THEN** the Radarr tile displays current queue/activity count from the Radarr API

#### Scenario: Widget gracefully handles unreachable service
- **WHEN** an arr service is down
- **THEN** the widget shows an error state without breaking the rest of the dashboard

### Requirement: Immich widget
The system SHALL configure a Homepage widget for Immich that displays photo/video statistics from the Immich API.

#### Scenario: Immich widget shows photo stats
- **WHEN** the dashboard loads and Immich is reachable
- **THEN** the Immich tile displays photo/video count statistics

### Requirement: API keys stored in SopsSecret
The system SHALL store all service API keys in an age-encrypted SopsSecret in the `homepage` namespace, exposed to the Homepage pod as environment variables with the `HOMEPAGE_VAR_` prefix.

#### Scenario: Keys available as environment variables
- **WHEN** the Homepage pod starts
- **THEN** environment variables HOMEPAGE_VAR_RADARR_KEY, HOMEPAGE_VAR_SONARR_KEY, HOMEPAGE_VAR_LIDARR_KEY, HOMEPAGE_VAR_READARR_KEY, HOMEPAGE_VAR_PROWLARR_KEY, and HOMEPAGE_VAR_IMMICH_KEY are set from the SopsSecret

### Requirement: Widget URLs use cluster-internal addresses
The system SHALL configure widget API URLs using cluster-internal service DNS (e.g., `http://radarr.media.svc.cluster.local:7878`) rather than external ingress URLs.

#### Scenario: Widget communicates internally
- **WHEN** Homepage fetches widget data
- **THEN** it connects to the service via cluster-internal DNS, not through Traefik
