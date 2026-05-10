## 1. Secrets

- [ ] 1.1 Re-encrypt Vaultwarden ADMIN_TOKEN from agenix to SOPS

## 2. Deploy

- [ ] 2.1 Deploy Vaultwarden to vaultwarden namespace via selfhosted.libsonnet (k8s)
- [ ] 2.2 Migrate data via rsync from /var/lib/vaultwarden (manual)
- [ ] 2.3 Add Vaultwarden PVC path to NAS Borg backup job (machines/nas/default.nix)
- [ ] 2.4 Verify Vaultwarden accessible and vault data intact
- [ ] 2.5 Verify Borg backup includes Vaultwarden data
