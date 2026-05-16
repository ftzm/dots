## Why

Immich is a multi-component app (server, ML, PostgreSQL with pgvecto.rs, Redis) that needs a Helm chart rather than selfhosted.libsonnet. It migrates independently from other services.

## What Changes

- Vendor Immich Helm chart and add to chartfile.yaml (k8s)
- Create narrow NFS PV for photos (`/pool-1/media/photos`) (k8s)
- Deploy Immich to `immich` namespace (k8s)
- Migrate data or re-scan photo library (manual)
- Remove Immich service from nuc config (NixOS)

## Capabilities

### New Capabilities
_(none — service migration)_

### Modified Capabilities
_(none)_

## Impact

- **k8s cluster repo**: Helm chart in chartfile.yaml, photos NFS PV, namespace
- **machines/nuc/default.nix**: Remove Immich service
