## 1. Merge repo

- [x] 1.1 Add cluster repo as git subtree under `cluster/` (preserving history)
- [x] 1.2 ~~Merge cluster flake.nix dev shell tools into dots flake.nix~~ Kept as sub-flake — `nix develop ./cluster` for cluster work

## 2. Update references

- [x] 2.1 Update ArgoCD Application source path to point at monorepo `cluster/` directory
- [x] 2.2 Update Renovate config paths (managerFilePatterns)
- [x] 2.3 ~~Update `just` commands if working directory changed~~ No changes needed — Justfile runs from `cluster/` with correct relative paths

## 3. Verify

- [x] 3.1 Verify ArgoCD syncs successfully from new path
- [x] 3.2 Verify `just render-lab` works from new location
- [x] 3.3 Verify nix develop shell has both NixOS and k8s tooling

## 4. Clean up

- [ ] 4.1 Delete standalone cluster repo (deferred to end of migration)
