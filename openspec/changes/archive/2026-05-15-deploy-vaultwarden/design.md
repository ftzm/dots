## Context

Vaultwarden is the password vault — highest-stakes migration. Needs SOPS secrets (re-encrypted from agenix) and must have backup configured before NixOS removal.

## Goals / Non-Goals

**Goals:**
- Vaultwarden running in k8s with SOPS-encrypted ADMIN_TOKEN
- Data migrated and verified
- Backup configured in NAS Borg job

**Non-Goals:**
- Changing Vaultwarden configuration (same settings, different runtime)

## Decisions

**Re-encrypt from agenix to SOPS rather than using both.** k8s uses sops-secrets-operator. Same age key, different envelope.

**Backup before removal is mandatory.** Verify Borg backup includes the new PVC path and test a restore before touching the NixOS service.

## Risks / Trade-offs

**Risk: Data loss** → Mitigation: rsync copy (not move), verify access in k8s, confirm Borg backup, only then remove NixOS service. This is the one migration where "verify twice, cut once" is non-negotiable.
