## Context

Two repos: `dots` (NixOS, comin) and `cluster` (k8s, Tanka/Jsonnet, ArgoCD). The migration needs atomic commits across both. Merging cluster into dots as a subdirectory gives one repo for everything.

## Goals / Non-Goals

**Goals:**
- All infrastructure in one repo
- ArgoCD still works (just different source path)
- Dev shell has both NixOS and k8s tooling
- Git history from cluster repo preserved

**Non-Goals:**
- Changing any k8s manifests or NixOS configs (just moving files)
- Changing the deployment mechanism (comin stays, ArgoCD stays)

## Decisions

**`git subtree add` to preserve cluster repo history.** Keeps the full commit history from the cluster repo as part of the monorepo history. Alternative `git merge --allow-unrelated-histories` with a subdirectory filter works too.

**ArgoCD source path update before deleting old repo.** ArgoCD must point at the new path in dots before the standalone cluster repo is removed, otherwise syncs break.

**Merge dev shells rather than nesting.** The dots flake already has a devShell. Add cluster's tools (jsonnet, tanka, just, etc.) to the same shell rather than having separate shells.

## Risks / Trade-offs

**Risk: ArgoCD sync breaks during cutover** → Mitigation: Update ArgoCD Application source path first, verify a sync succeeds, then delete the old repo.

**Risk: Renovate config breaks** → Mitigation: Update `managerFilePatterns` to match new paths under `cluster/`. Test with a dry run.
