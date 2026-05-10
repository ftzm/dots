## Why

The migration plan requires changes to both NixOS configs (dots repo) and k8s manifests (cluster repo). Having them in separate repos means no atomic commits across both, no unified git history, and Claude can't see all infrastructure at once. Merging the cluster repo into dots is a prerequisite for all subsequent k8s changes.

## What Changes

- Move cluster repo contents into `dots/cluster/`
- Merge cluster's `flake.nix` dev shell into dots' `flake.nix`
- Update ArgoCD `Application` source to point at the monorepo
- Update Renovate config paths
- Update `just` commands for the new working directory
- Delete the standalone cluster repo (after verification)

## Capabilities

### New Capabilities
_(none — repo reorganization)_

### Modified Capabilities
_(none)_

## Impact

- **flake.nix**: Merge dev shell inputs/outputs from cluster flake
- **cluster/**: New directory containing all k8s manifests, libs, charts, Justfile
- **ArgoCD**: Application source path change (must deploy before deleting old repo)
- **Renovate**: Config path updates for new monorepo structure
- **comin**: URL stays the same (same repo)
