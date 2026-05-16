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

### No Guessing — ABSOLUTE RULE

NEVER use words like "likely", "probably", "might", "could be", "I think" when describing what is happening in the system. Every single claim about system state must cite the specific evidence: the exact log line, the exact command output, the exact source code line. If you have not verified something, do NOT state it — instead say "I don't know yet, let me check" and then CHECK.

This applies to EVERY statement, no exceptions. Not "the file was likely lost" — instead, check whether the file exists. Not "the config is probably wrong" — instead, read the config. Not "it might be a version issue" — instead, check the version. NEVER speculate. ALWAYS verify first, then state facts with evidence.

### Diagnose Before Fixing

When encountering a service error (crash loop, health check failure, etc.), the FIRST priority is to understand WHY it's happening — not to restart the pod or delete it. Restarting without understanding the cause means the problem will return. Follow this order:

1. Read the logs to understand the failure
2. Identify the root cause
3. Determine a permanent fix (config change, resource adjustment, etc.)
4. Implement the fix declaratively
5. Only THEN restart if needed

Never `kubectl delete pod` or `systemctl restart` as a first reaction. That's treating symptoms, not causes.
