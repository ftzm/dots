## Context

Immich is multi-component (server, ML, PostgreSQL with pgvecto.rs, Redis). The official Helm chart manages this complexity. It's independent from other services.

## Goals / Non-Goals

**Goals:**
- Immich running in k8s `immich` namespace via Helm chart
- Photos accessible via narrow NFS PV

**Non-Goals:**
- GPU acceleration (not using GPU, confirmed in migration plan)

## Decisions

**Helm chart over selfhosted.libsonnet.** Immich has multiple components with complex inter-service communication. The chart handles this.

**Re-scan library rather than migrate PostgreSQL.** The photos are on NFS and don't move. Re-scanning is simpler than migrating a pgvecto.rs database.

## Risks / Trade-offs

**Risk: Re-scan loses metadata (faces, albums)** → Acceptable trade-off vs PostgreSQL migration complexity. User can re-tag if needed.
