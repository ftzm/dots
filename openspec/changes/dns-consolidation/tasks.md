## 1. Blocky config

- [ ] 1.1 Update Blocky customDNS for all *.lan.ftzmlab.xyz → 100.64.0.2 (k8s)
- [ ] 1.2 Verify public *.ftzmlab.xyz resolves via Cloudflare

## 2. LAN cutover

- [ ] 2.1 Update DHCP to point LAN clients at Blocky
- [ ] 2.2 Verify ad-blocking and custom DNS resolution

## 3. Clean up

- [ ] 3.1 Remove Pi-hole from pi config (machines/pi/default.nix)
- [ ] 3.2 Evaluate if pi machine is still needed
- [ ] 3.3 Verify pi config evaluates cleanly
