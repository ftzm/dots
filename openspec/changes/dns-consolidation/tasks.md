## 1. Blocky config

- [x] 1.1 Update Blocky customDNS for all *.lan.ftzmlab.xyz domains (k8s)
- [x] 1.2 Ensure Blocky is reachable from LAN, Tailscale, and WG networks (NodePort or hostPort)
- [x] 1.3 Verify public *.ftzmlab.xyz resolves via Cloudflare

## 2. Client DNS configuration

- [ ] 2.1 Update DHCP to point LAN clients at Blocky
- [x] 2.2 Configure Tailscale split DNS for *.lan.ftzmlab.xyz → Blocky
- [ ] 2.3 Configure WireGuard client DNS to use Blocky
- [ ] 2.4 Verify resolution from a LAN client
- [ ] 2.5 Verify resolution from a Tailscale client
- [ ] 2.6 Verify resolution from a WG client

## 3. Router port-forward

- [ ] 3.1 Update router NAT: forward 80/443 → nuc:9080/9443 for Jellyfin public access
- [ ] 3.2 Verify Jellyfin accessible from internet

## 4. Clean up

- [x] 4.1 Remove Pi-hole from pi config (machines/pi/default.nix)
- [x] 4.2 Evaluate if pi machine is still needed (yes: ddclient + headscale remain)
- [x] 4.3 Verify pi config evaluates cleanly
