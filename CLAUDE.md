## Infrastructure Principles

This is a declarative infrastructure repo. NixOS configs deploy via comin, k8s manifests deploy via ArgoCD. Every change must flow through git → automated deployment.

### No Manual Actions

NEVER run manual `kubectl apply`, `ssh ... systemctl`, or any imperative fix as a substitute for declarative config. If something doesn't deploy correctly:

1. Investigate the root cause (why isn't the automation handling it?)
2. Fix it in the declarative config (NixOS module, Jsonnet, Helm values, ArgoCD config)
3. If a resource type or pattern doesn't work with the deployment system, change the approach — don't work around it manually

If you find yourself about to type `kubectl apply` for anything other than one-time bootstrapping (like the initial ArgoCD Application cutover), STOP and find the declarative solution.

### Patterns Over One-Offs

When solving a problem, create a reusable pattern. If multiple services need the same config shape, create a helper (libsonnet, NixOS module). Future changes should be able to follow established patterns with minimal thought.

### Verify End-to-End

After making changes, verify they work through the actual deployment pipeline (comin deploy, ArgoCD sync), not just `nix eval` or `just render-lab`. The deployment pipeline IS the test.
