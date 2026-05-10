## Context

With Traefik handling all routing (done in add-traefik-ingress), nginx only needs to remain for WebDAV verb handling. All other virtualHosts, the WG catch-all, and ACME config can be removed.

## Goals / Non-Goals

**Goals:**
- Remove all nginx virtualHosts except WebDAV
- Remove ACME config (cert-manager handles everything)
- WebDAV nginx listens on 127.0.0.1 only, fronted by Traefik

**Non-Goals:**
- Removing nginx entirely (still needed for WebDAV DAV verbs)

## Decisions

**Keep nginx for WebDAV only on localhost.** Traefik doesn't handle DAV verbs natively. nginx on 127.0.0.1 handles the verbs, Traefik routes to it.

## Risks / Trade-offs

**Risk: Breaking WebDAV** → Mitigation: Verify WebDAV works through Traefik → nginx(localhost) before removing other virtualHosts.
