## Context

DNS is split between Pi-hole (pi) and Blocky (k8s). With all services behind Traefik, Blocky needs the full set of `*.lan.ftzmlab.xyz` mappings and Pi-hole can be retired.

## Goals / Non-Goals

**Goals:**
- All `*.lan.ftzmlab.xyz` domains resolve via Blocky
- Public DNS works via Cloudflare
- Pi-hole removed

**Non-Goals:**
- Migrating Pi-hole allowlist/blocklist customizations (start fresh with Blocky defaults)
- Moving ddclient off the pi (evaluate only)

## Decisions

**DHCP cutover to Blocky during low-usage window.** DNS affects all LAN clients. Careful timing.

**Keep ddclient on pi for now.** It's the only remaining reason for the pi. Moving it is trivial but not urgent.

## Risks / Trade-offs

**Risk: DNS misconfiguration causes total outage** → Mitigation: Keep Pi-hole running until Blocky is verified. Test resolution from multiple clients. Have rollback plan (re-enable Pi-hole in DHCP).
