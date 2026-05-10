## Why

Host logs and metrics from nuc and nas aren't visible in k8s Grafana. Adding ingestion now means we can verify host observability works before tearing down the NixOS stack in a separate change.

## What Changes

- Add hostPath volume mount of `/var/log/journal` to Alloy DaemonSet and `loki.source.journal` block (k8s)
- Create private IngressRoute for `loki.lan.ftzmlab.xyz` (k8s)
- Repoint nas Promtail to push to `https://loki.lan.ftzmlab.xyz/loki/api/v1/push` (NixOS)
- Add nuc and nas node-exporter scrape targets to kube-prometheus-stack (k8s)

## Capabilities

### New Capabilities
_(none — adding ingestion to existing k8s observability stack)_

### Modified Capabilities
_(none)_

## Impact

- **k8s cluster repo**: Alloy config, kube-prometheus-stack values, new IngressRoute
- **machines/nas/default.nix**: Promtail push URL change
