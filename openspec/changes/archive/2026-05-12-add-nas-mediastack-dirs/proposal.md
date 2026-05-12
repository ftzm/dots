## Why

The arr stack needs the mediastack directory tree to exist on the NAS before pods can mount it. Creating directories declaratively via tmpfiles.rules is safe and idempotent — no effect until k8s apps actually use them.

## What Changes

- Add systemd.tmpfiles.rules for the full mediastack directory tree (`/pool-1/mediastack/...`)
- Add tmpfiles rules for other managed directories (`/pool-1/media/photos`, `/pool-1/cloud`)
- Add recursive permission enforcement on download directories

## Capabilities

### New Capabilities
_(none — filesystem preparation)_

### Modified Capabilities
_(none)_

## Impact

- **machines/nas/default.nix**: Add tmpfiles.rules block
