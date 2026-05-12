## Context

Three client access paths exist, each needing `*.lan.ftzmlab.xyz` to resolve to a different Traefik entrypoint IP:

| Client path | Needs to resolve to | Traefik entrypoint |
|-------------|--------------------|--------------------|
| LAN | 192.168.1.4 (nuc LAN IP) | privateweb/privatesecure |
| Tailscale | 100.64.0.2 (Tailscale IP) | privateweb/privatesecure |
| WireGuard | 10.0.100.4 (WG IP) | wgweb/wgsecure |

Jellyfin is the only public service (`jellyfin.ftzmlab.xyz`), served on public entrypoints at 192.168.1.4:9080/9443.

## Goals / Non-Goals

**Goals:**
- All client paths can reach `*.lan.ftzmlab.xyz` services
- Jellyfin accessible from the internet
- Blocky handles all DNS (replaces Pi-hole)
- Single source of truth for domain→IP mappings

**Non-Goals:**
- DNSSEC
- Migrating Pi-hole allowlists/blocklists

## Decisions

**Blocky as the DNS server for all paths.**
Blocky already runs in k8s. It can serve different responses based on client subnet using conditional upstream or customDNS with client groups. LAN clients use it via DHCP. Tailscale clients use it via Tailscale split DNS. WG clients use it via WG DNS config.

**However — Blocky needs to be reachable from all three networks.** Currently it's a k8s ClusterIP. Options:
1. NodePort on all interfaces (like we did for Loki)
2. Blocky already has a LoadBalancer or hostPort config

Need to check current Blocky exposure.

**Router port-forward update for Jellyfin.**
Router currently forwards 80/443 to nuc. Public entrypoint is now 9080/9443. Update the router NAT rules.

**Tailscale split DNS.**
Tailscale admin console → DNS → add nameserver for `lan.ftzmlab.xyz` pointing at Blocky's Tailscale-reachable IP.

**WG DNS.**
WG clients already have the nuc as a peer. If Blocky is reachable on the WG IP, add it as DNS in the WG client config.

## Risks / Trade-offs

**Risk: DNS is critical infrastructure** → Mitigation: Keep Pi-hole running until Blocky is verified from all access paths. Test from each client type before cutover.

**Risk: Split-horizon DNS complexity** → Mitigation: If Blocky can't do client-based responses, use a single IP reachable from all networks (the nuc's LAN IP, which LAN/WG/Tailscale can all reach if routing allows).
