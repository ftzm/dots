## 1. Log ingestion

- [x] 1.1 Add hostPath volume `/var/log/journal` to Alloy DaemonSet (k8s)
- [x] 1.2 Add `loki.source.journal` block to Alloy config (k8s)
- [x] 1.3 Create private IngressRoute for `loki.lan.ftzmlab.xyz` (k8s)
- [x] 1.4 Add Promtail to nas pushing to k8s Loki (machines/nas/default.nix)
- [ ] 1.5 Verify nuc and nas logs appear in k8s Grafana

## 2. Metric ingestion

- [x] 2.1 Add scrape targets for nuc (192.168.1.4:9002) and nas (192.168.1.3:9002) to kube-prometheus-stack (k8s)
- [ ] 2.2 Verify node-exporter metrics from both hosts in k8s Prometheus
