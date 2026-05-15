## Context

After deploy-arr-stack, the arr apps and download clients run in k8s. The NixOS service definitions are redundant. Jellyfin stays on NixOS but its library paths need updating to the new mediastack mount.

## Goals / Non-Goals

**Goals:**
- Remove arr service blocks from nuc config
- Update Jellyfin library paths

**Non-Goals:**
- Touching k8s deployments

## Decisions

**Remove all arr services in one commit.** They migrated as a batch and should be removed as a batch.

**Update Jellyfin paths in the same commit.** The path change is a direct consequence of the mediastack migration. Logically coupled.

## Risks / Trade-offs

**Risk: Jellyfin path change breaks library** → Verify the new NFS mount path exists and is readable by Jellyfin before deploying.
