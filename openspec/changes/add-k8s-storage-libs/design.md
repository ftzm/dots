## Context

Multiple k8s services need similar deployment patterns (Deployment + Service + PVC + IngressRoute) and shared storage (mediastack NFS mount, narrow NFS mounts). Library files avoid repetition.

## Goals / Non-Goals

**Goals:**
- selfhosted.libsonnet: stamp out a standard app in ~10 lines
- storage.libsonnet: encode NFS config, PV/PVC helpers, pipeline contract

**Non-Goals:**
- Deploying any apps (just the libraries)

## Decisions

**Jsonnet helpers over Helm charts.** These are simple single-container apps. A Jsonnet function is more maintainable than vendoring a Helm chart per app.

**Pipeline contract in storage.libsonnet.** The `appAccess` map documents which apps read/write which paths. Readable top-to-bottom for verification.

## Risks / Trade-offs

**Risk: Helper doesn't cover all app needs** → Mitigation: Jsonnet's `+:` operator lets any app override or extend the helper output for one-off needs.
