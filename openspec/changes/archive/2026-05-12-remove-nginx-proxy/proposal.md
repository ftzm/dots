## Why

With Traefik IngressRoutes handling all routing (done in add-traefik-ingress), the nginx reverse proxy and ACME config on the nuc are redundant. nginx stays only for WebDAV verb handling, listening on localhost behind Traefik.

## What Changes

- Change WebDAV nginx to listen on 127.0.0.1 only
- Remove all nginx virtualHosts except WebDAV backend
- Remove nginx WG catch-all proxy
- Remove `security.acme` config

## Capabilities

### New Capabilities
_(none — pure removal/reconfiguration)_

### Modified Capabilities
_(none)_

## Impact

- **machines/nuc/default.nix**: Remove virtualHosts, ACME config, WG catch-all; modify WebDAV listen address
