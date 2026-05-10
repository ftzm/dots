## Why

The arr stack and independent services need reusable k8s deployment and storage patterns. Creating the library files first establishes the foundation before any apps are deployed.

## What Changes

- Create `selfhosted.libsonnet` — reusable helper for Deployment + Service + PVC + IngressRoute
- Create `storage.libsonnet` — NFS config, mediastack PV/PVC, narrow PV helpers, pipeline contract

## Capabilities

### New Capabilities
_(none — library code, no runtime effect)_

### Modified Capabilities
_(none)_

## Impact

- **k8s cluster repo**: Two new library files under `lib/`
