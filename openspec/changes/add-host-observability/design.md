## Context

k8s runs Loki (via Alloy) and Prometheus (via kube-prometheus-stack) but only scrapes k8s workloads. Nuc is a k3s node so its journal is locally mountable. Nas is not a k3s node and must push over the network.

## Goals / Non-Goals

**Goals:**
- Nuc and nas host logs in k8s Loki
- Nuc and nas node-exporter metrics in k8s Prometheus

**Non-Goals:**
- Removing NixOS observability (separate change)
- Dashboard creation

## Decisions

**Alloy hostPath for nuc journal.** Alloy already runs as a DaemonSet. A `loki.source.journal` block + hostPath volume is minimal config.

**Keep Promtail on nas, repoint to k8s Loki.** Nas isn't a k3s node. Changing the push URL is simpler than joining it to the cluster.

**Private IngressRoute for Loki, no auth.** Only reachable from nas over Tailscale/WG. Private entrypoints only.

## Risks / Trade-offs

**Risk: nas Promtail can't reach Loki** → Test with curl from nas before switching config.
