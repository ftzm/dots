## Context

The mediastack directory tree must exist on the NAS before k8s pods can use it. systemd.tmpfiles.rules is declarative and idempotent.

## Goals / Non-Goals

**Goals:**
- Create `/pool-1/mediastack/` directory tree with correct ownership (root:storage/1001)
- Create other managed directories (`/pool-1/media/photos`, `/pool-1/cloud`)
- Recursive permission enforcement on download directories

**Non-Goals:**
- Changing NFS exports (root export already covers mediastack path)
- Creating k8s PVs (that's in storage libs)

## Decisions

**`d` rules for directory creation, `Z` rules only on download dirs.** `Z` walks recursively — applying it to the entire media library would be slow and unnecessary. Only download dirs need it (files arrive with varying permissions from Deluge/NZBget).

## Risks / Trade-offs

**Risk: None significant.** tmpfiles.rules only create directories and fix permissions. Safe, no-op if dirs already exist.
