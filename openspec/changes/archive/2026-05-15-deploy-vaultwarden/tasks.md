## 1. Secrets

- [x] 1.1 Re-encrypt Vaultwarden ADMIN_TOKEN from agenix to SOPS

## 2. Deploy

- [x] 2.1 Deploy Vaultwarden to vaultwarden namespace via selfhosted.libsonnet (k8s)
- [x] 2.2 Migrate data via rsync from /var/lib/vaultwarden (manual)
- [x] 2.3 Add Vaultwarden to NAS Borg backup (static NFS PV at /pool-1/vaultwarden)
- [x] 2.4 Verify Vaultwarden accessible and vault data intact
- [x] 2.5 Verify Borg backup includes Vaultwarden data
