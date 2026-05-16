## Context

After deploy-immich, deploy-vaultwarden, and deploy-simple-services, six services are verified running in k8s. Their NixOS definitions can be removed.

## Goals / Non-Goals

**Goals:**
- Remove all six service/container blocks from nuc config

**Non-Goals:**
- Touching k8s deployments

## Decisions

**Remove all in one commit after all are verified.** Batching the removal is simpler than six individual commits for straightforward deletions.

## Risks / Trade-offs

**Risk: Removing a service that isn't actually verified** → Mitigation: Check each k8s deployment is healthy before this commit.
