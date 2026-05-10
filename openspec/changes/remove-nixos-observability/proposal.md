## Why

With host logs and metrics now ingested by k8s (done in add-host-observability), the NixOS Loki, Promtail, and Grafana on the nuc are redundant. Remove them to reduce config surface.

## What Changes

- Remove Loki service from nuc config
- Remove Promtail service from nuc config
- Remove Grafana service from nuc config

## Capabilities

### New Capabilities
_(none — pure removal)_

### Modified Capabilities
_(none)_

## Impact

- **machines/nuc/default.nix**: Remove three service blocks
