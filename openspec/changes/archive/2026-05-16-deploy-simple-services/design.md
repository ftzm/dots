## Context

Four simple single-container services: Navidrome, Audiobookshelf, The Lounge, Filestash. All use selfhosted.libsonnet. Each gets its own namespace and narrow NFS PV where needed.

## Goals / Non-Goals

**Goals:**
- All four services running in k8s
- Data migrated for Navidrome, Audiobookshelf, The Lounge
- Filestash starts fresh (stateless config)

**Non-Goals:**
- Removing NixOS services (separate change)

## Decisions

**Deploy all four in one commit.** They're independent but similarly structured. One commit with four selfhosted.libsonnet calls is clean and reviewable.

**Narrow NFS PVs, not mediastack.** Each app only needs its specific directory. Minimal blast radius.

## Risks / Trade-offs

**Risk: Low.** These are simple apps with SQLite or file-based state. rsync + verify is straightforward.
