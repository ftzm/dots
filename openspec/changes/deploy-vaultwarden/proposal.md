## Why

Vaultwarden is the password vault — the most critical service to migrate carefully. It needs SOPS-encrypted secrets and must have backup configured before the NixOS service is removed.

## What Changes

- Re-encrypt Vaultwarden ADMIN_TOKEN from agenix to SOPS (k8s)
- Deploy Vaultwarden to `vaultwarden` namespace via selfhosted.libsonnet (k8s)
- Migrate data via rsync from `/var/lib/vaultwarden` (manual)
- Add Vaultwarden PVC path to NAS Borg backup job (NixOS)
- Remove Vaultwarden service from nuc config after verification (NixOS)

## Capabilities

### New Capabilities
_(none — service migration)_

### Modified Capabilities
_(none)_

## Impact

- **k8s cluster repo**: Vaultwarden deployment, SOPS secret
- **machines/nuc/default.nix**: Remove Vaultwarden service
- **machines/nas/default.nix**: Add PVC path to Borg backup
- **Risk**: Password vault — verify backup and access before removing NixOS service
