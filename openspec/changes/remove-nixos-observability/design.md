## Context

After add-host-observability, nuc and nas logs/metrics flow through k8s. The NixOS Loki, Promtail, and Grafana on the nuc are now redundant.

## Goals / Non-Goals

**Goals:**
- Remove Loki, Promtail, Grafana service blocks from nuc config

**Non-Goals:**
- Touching k8s observability config
- Removing node-exporter (still needed for host metrics)

## Decisions

**Remove all three in one commit.** They're all part of the same redundant stack. No reason to sequence them.

## Risks / Trade-offs

**Risk: Removing Grafana loses custom dashboards** → Mitigation: Check if any custom dashboards exist on NixOS Grafana before removal. Recreate in k8s Grafana if needed.
