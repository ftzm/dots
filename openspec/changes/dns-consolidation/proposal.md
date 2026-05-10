## Why

With all services behind Traefik, DNS needs updating for the new `*.lan.ftzmlab.xyz` domains. Blocky in k8s replaces Pi-hole on the pi for LAN DNS and ad-blocking.

## What Changes

- Update Blocky customDNS mappings for all `*.lan.ftzmlab.xyz` → 100.64.0.2 (k8s)
- Verify public `*.ftzmlab.xyz` resolves via Cloudflare
- Update DHCP to point LAN clients at Blocky
- Remove Pi-hole from pi config (NixOS)
- Evaluate if pi machine is still needed

## Capabilities

### New Capabilities
_(none — DNS consolidation)_

### Modified Capabilities
_(none)_

## Impact

- **k8s cluster repo**: Blocky customDNS config
- **machines/pi/default.nix**: Remove Pi-hole
- **Network**: DHCP DNS server change affects all LAN clients
