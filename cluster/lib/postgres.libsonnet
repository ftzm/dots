{
  // CloudNativePG image handling.
  //
  // Clusters reference a ClusterImageCatalog by PostgreSQL major rather than
  // naming an image directly. That separates two things which look identical
  // in a tag but are not remotely alike:
  //
  //   patch within a major  ->  routine, safe to take unattended
  //   change of major       ->  the operator shuts the cluster down and runs
  //                             pg_upgrade --link, destroying replica PVCs
  //
  // With a bare `imageName`, both are "a docker tag changed" and nothing
  // distinguishes them. With a catalog, moving majors is an explicit edit of
  // `major:` at the call site and can never happen as a side effect of a
  // version bump.

  // PostgreSQL major from an image ref. Handles both plain CNPG tags
  // ('...postgresql:18.2' -> 18) and vectorchord's coupled
  // '<postgres>-<extension>' form ('...vectorchord:16.9-0.4.3' -> 16).
  // Takes the last colon-separated segment so a registry port cannot confuse
  // it.
  majorOf(image)::
    local parts = std.split(image, ':');
    local tag = parts[std.length(parts) - 1];
    std.parseInt(std.split(std.split(tag, '.')[0], '-')[0]),

  // A cluster-scoped catalog, so one definition serves Clusters in any
  // namespace. `imageList` is a plain array of image refs; each entry's major
  // is derived from its own tag, so the declared major and the image can
  // never drift apart.
  clusterImageCatalog(name, imageList):: {
    apiVersion: 'postgresql.cnpg.io/v1',
    kind: 'ClusterImageCatalog',
    metadata: { name: name },
    spec: {
      images: [
        { major: $.majorOf(image), image: image }
        for image in imageList
      ],
    },
  },

  // Reference for a Cluster's spec.imageCatalogRef. `major` is written out
  // literally at the call site on purpose: it is the one place a major
  // version upgrade is chosen, and it should read as a decision.
  //
  // If a bump ever moves an image across majors, the catalog stops offering
  // the major a Cluster asks for. The operator then reports an error and
  // leaves the running database alone, rather than quietly migrating it.
  catalogRef(name, major):: {
    apiGroup: 'postgresql.cnpg.io',
    kind: 'ClusterImageCatalog',
    name: name,
    major: major,
  },
}
