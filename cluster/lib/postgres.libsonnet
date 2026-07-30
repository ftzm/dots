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

  // ---------------------------------------------------------------------
  // Major upgrade harness
  //
  // Changing a Cluster's `major` makes the operator stop the database and run
  // pg_upgrade --link. Two things it will not do for you:
  //
  //   "CloudNativePG is not responsible for PostgreSQL extensions. You must
  //    ensure that extensions in the source PostgreSQL image are compatible
  //    with those in the target image."
  //
  // and it does not carry optimizer statistics across, so the first queries
  // after an upgrade plan against nothing.
  //
  // These two hooks bracket the sync that performs the upgrade. Both are
  // written to be cheap no-ops on an ordinary sync, because ArgoCD runs hooks
  // on *every* sync -- they decide what to do by comparing the running server
  // to the target, not by assuming an upgrade is happening.

  // Shared env for a job that talks to the cluster as the application user.
  local pgEnv(host, user, database, secretName, secretKey) = [
    { name: 'PGHOST', value: host },
    { name: 'PGUSER', value: user },
    { name: 'PGDATABASE', value: database },
    { name: 'PGPASSWORD', valueFrom: { secretKeyRef: { name: secretName, key: secretKey } } },
  ],

  // uid 26 (the CNPG image's user) needs the storage group to write to the
  // NFS backup dir. Same reasoning as lib/backup.libsonnet -- omitting it
  // fails with EACCES.
  local backupJob(name, ns, image, script, env, pvcName, annotations) = {
    apiVersion: 'batch/v1',
    kind: 'Job',
    metadata: { name: name, namespace: ns, annotations: annotations },
    spec: {
      // A safety gate must not be retried into passing.
      backoffLimit: 0,
      template: { spec: {
        restartPolicy: 'Never',
        securityContext: { supplementalGroups: [1001] },
        containers: [{
          name: name,
          image: image,
          command: ['/bin/sh', '-c'],
          args: [script],
          env: env,
          volumeMounts: [{ name: 'backup', mountPath: '/backup' }],
        }],
        volumes: [{ name: 'backup', persistentVolumeClaim: { claimName: pvcName } }],
      } },
    },
  },

  // PreSync: if the running major already equals the target there is no
  // upgrade pending and this exits immediately. If they differ, it takes a
  // dedicated pre-upgrade dump and verifies it is actually restorable. A
  // failure here fails the sync, so the image change never reaches the
  // cluster without a good dump behind it.
  //
  // This is the gate that matters: once pg_upgrade --link succeeds, the old
  // data directory shares inodes with the new one and is no longer a fallback.
  // This dump is the only way back.
  majorUpgradeGate(name, ns, image, host, user, database, secretName, pvcName, targetMajor, secretKey='password')::
    backupJob(
      name, ns, image,
      |||
        set -eu
        running=$(psql -tAc 'SHOW server_version_num' | tr -d '[:space:]')
        major=$(( running / 10000 ))
        if [ "$major" = "%(target)s" ]; then
          echo "running PostgreSQL major $major already matches target %(target)s - no upgrade pending"
          exit 0
        fi
        echo "MAJOR UPGRADE PENDING: PostgreSQL $major -> %(target)s"
        out="/backup/%(name)s-pg$major-to-%(target)s-$(date +%%Y%%m%%d-%%H%%M%%S).dump"
        echo "taking pre-upgrade dump to $out"
        pg_dump --format=custom --file="$out"
        [ -s "$out" ] || { echo "FATAL: pre-upgrade dump is empty"; exit 1; }
        pg_restore --list "$out" > /dev/null || { echo "FATAL: pre-upgrade dump is not readable"; exit 1; }
        echo "pre-upgrade dump verified: $(wc -c < "$out") bytes"
      ||| % { target: std.toString(targetMajor), name: name },
      pgEnv(host, user, database, secretName, secretKey),
      pvcName,
      {
        'argocd.argoproj.io/hook': 'PreSync',
        'argocd.argoproj.io/hook-delete-policy': 'BeforeHookCreation',
      },
    ),

  // PostSync: runs after the sync is applied and resources report healthy.
  //
  // Detects an upgrade by comparing the running major against a marker on the
  // backup volume rather than against the target, so it reacts to what
  // actually happened. On first run it records the baseline and does nothing,
  // which keeps it from analyzing a database that was never upgraded.
  majorUpgradeFinalize(name, ns, image, host, user, database, secretName, pvcName, secretKey='password')::
    backupJob(
      name, ns, image,
      |||
        set -eu
        running=$(psql -tAc 'SHOW server_version_num' | tr -d '[:space:]')
        major=$(( running / 10000 ))
        marker="/backup/.pg-major-%(db)s"
        if [ ! -f "$marker" ]; then
          echo "$major" > "$marker"
          echo "baseline recorded: PostgreSQL $major (nothing to finalize)"
          exit 0
        fi
        last=$(cat "$marker")
        if [ "$last" = "$major" ]; then
          echo "PostgreSQL major unchanged ($major) - nothing to finalize"
          exit 0
        fi
        echo "PostgreSQL major changed $last -> $major; finalizing"
        # pg_upgrade does not carry optimizer statistics across. Staged so the
        # database becomes usable quickly rather than after one long pass.
        vacuumdb --analyze-in-stages --dbname="$PGDATABASE"
        echo "$major" > "$marker"
        echo "finalized: statistics rebuilt for PostgreSQL $major"
      ||| % { db: database },
      pgEnv(host, user, database, secretName, secretKey),
      pvcName,
      {
        'argocd.argoproj.io/hook': 'PostSync',
        'argocd.argoproj.io/hook-delete-policy': 'BeforeHookCreation',
      },
    ),

  // Extensions the operator should keep current, for the ones this repo
  // created rather than the application. Omitting `version` means "the default
  // version in the image's control file", so after a major upgrade brings a
  // newer build the operator issues the ALTER EXTENSION UPDATE itself -- with
  // its own privileges, which matters because these are postgres-owned and the
  // application role cannot update them.
  //
  // CloudNativePG reconciles only the extensions listed here, so extensions the
  // application manages through its own migrations are left alone.
  managedExtensions(name, ns, cluster, database, owner, extensions):: {
    apiVersion: 'postgresql.cnpg.io/v1',
    kind: 'Database',
    metadata: { name: name, namespace: ns },
    spec: {
      cluster: { name: cluster },
      name: database,
      owner: owner,
      ensure: 'present',
      // Default, but state it: this must never be able to drop the database.
      databaseReclaimPolicy: 'retain',
      extensions: [{ name: e, ensure: 'present' } for e in extensions],
    },
  },
}
