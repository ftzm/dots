## Why

With all services behind Traefik, DNS needs updating for the new `*.lan.ftzmlab.xyz` domains. Different access paths (LAN, Tailscale, WireGuard) need to resolve these domains to different IPs. The router port-forward needs updating for the new public entrypoint port. The old `*.ftzmlab.xyz` private service domains need redirecting to the new `*.lan.ftzmlab.xyz` names.

## What Changes

- Update Blocky customDNS mappings for `*.lan.ftzmlab.xyz` (k8s)
- Configure Blocky to return different IPs based on client source (LAN vs Tailscale vs WG), or use a single IP reachable from all networks
- Configure Tailscale split DNS to use Blocky for `*.lan.ftzmlab.xyz`
- Configure WireGuard client DNS to use Blocky
- Update DHCP to point LAN clients at Blocky
- Update router port-forward: 80/443 → nuc:9080/9443 for Jellyfin public access
- Verify public `*.ftzmlab.xyz` resolves via Cloudflare
- Remove Pi-hole from pi config
- Evaluate if pi machine is still needed

## Capabilities

### New Capabilities
_(none — DNS consolidation and access routing)_

### Modified Capabilities
_(none)_

## Impact

- **k8s cluster repo**: Blocky customDNS config
- **machines/pi/default.nix**: Remove Pi-hole
- **Network**: DHCP DNS change, router port-forward change, Tailscale DNS config
- **Clients**: All devices need to resolve `*.lan.ftzmlab.xyz` correctly for their network path
- **Bookmarks**: Old `*.ftzmlab.xyz` URLs for private services will stop working after nginx removal
