## Context

The nuc (`machines/nuc/default.nix`) accumulates service definitions over time. Several are dead: containers that were replaced or abandoned, a disabled Prometheus block, and nginx virtualHosts for defunct apps. All six targets live in a single file.

## Goals / Non-Goals

**Goals:**
- Remove all dead service config from `machines/nuc/default.nix`
- Ensure the nuc NixOS config still evaluates cleanly after removal

**Non-Goals:**
- Removing DNS records or firewall rules that reference these services (Phase 5)
- Migrating any data from these services
- Touching any live services

## Decisions

**Delete entire config blocks rather than commenting out.**
These services have confirmed replacements or are abandoned. No reason to keep dead code. Git history preserves everything if needed.

**Remove in a single commit.**
All six targets are in one file and none depend on each other. A single atomic commit is simplest.

## Risks / Trade-offs

**Risk: Accidentally removing config used by another service** → Mitigation: Each removal target is a self-contained block (OCI container definition, systemd service, nginx virtualHost, or Prometheus config). No other config references these blocks. Verify with a `nix eval` or build after editing.

**Risk: Orphaned state on disk after deploy** → Mitigation: OCI container images and volumes will remain on disk after the NixOS rebuild removes their definitions. This is harmless; they can be cleaned up manually with `podman system prune` if desired.
