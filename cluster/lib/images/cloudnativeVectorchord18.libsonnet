// Immich's database. Only the major actually in use is listed: a catalog
// entry for a major nothing runs is an invitation to set a Cluster back to
// it, and CloudNativePG rejects a downgrade only after the operator has
// already tried. Add the next major as its own file and images.libsonnet
// entry when an upgrade is deliberately started, and remove the old one once
// it has proven itself.
'ghcr.io/tensorchord/cloudnative-vectorchord:18.6-1.1.1'
