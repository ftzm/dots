## 1. Blocky config

- [x] 1.1 Update Blocky customDNS for all *.lan.ftzmlab.xyz domains (k8s)
- [x] 1.2 Ensure Blocky is reachable from LAN, Tailscale, and WG networks (NodePort or hostPort)
- [x] 1.3 Verify public *.ftzmlab.xyz resolves via Cloudflare

## 2. Client DNS configuration

- [x] 2.1 Update DHCP to point LAN clients at Blocky
- [x] 2.2 Configure Tailscale split DNS for *.lan.ftzmlab.xyz → Blocky
- [x] 2.3 Configure WireGuard client DNS to use Blocky (N/A: mobile uses Tailscale, eachtrai uses Tailscale primarily)
- [x] 2.4 Verify resolution from a LAN client
- [x] 2.5 Verify resolution from a Tailscale client
- [x] 2.6 Verify resolution from a WG client (N/A: WG clients use Tailscale for DNS)

## 3. Router port-forward

- [x] 3.1 Update Traefik public entrypoints to 80/443
- [x] 3.2 Verify Jellyfin accessible from internet

## 4. Clean up

- [x] 4.1 Remove Pi-hole from pi config (machines/pi/default.nix)
- [x] 4.2 Evaluate if pi machine is still needed (yes: ddclient + headscale remain)
- [x] 4.3 Verify pi config evaluates cleanly
