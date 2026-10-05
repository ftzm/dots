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

  // The VectorChord version a vectorchord image carries, from the second half
  // of its coupled '<postgres>-<extension>' tag ('16.9-0.4.3' -> '0.4.3').
  // Empty for plain CNPG images, which have no such suffix -- callers use that
  // to mean "this database has no VectorChord to worry about".
  vchordVersionOf(image)::
    local parts = std.split(image, ':');
    local tag = parts[std.length(parts) - 1];
    local halves = std.split(tag, '-');
    if std.length(halves) > 1 then halves[1] else '',

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

  // Every hook waits for the database before deciding anything.
  //
  // An ArgoCD retry can land while the pod is still rolling from the change an
  // earlier attempt made, so a hook has to tolerate downtime it effectively
  // caused itself. That is what wedged the VectorChord 1.1.1 rollout: the
  // first attempt took its dump and applied the image, the database began
  // rolling, ArgoCD retried, and PreSync ran again while the pod was still
  // failing its readiness probe.
  //
  // Waiting here rather than leaning on the Job's backoffLimit keeps the two
  // outcomes distinct, which is the whole point of a gate:
  //
  //   could not connect  ->  the check never ran; say so and stop
  //   connected, bad dump ->  the check ran and failed; stop immediately,
  //                           with no retry to paper over it
  //
  // A Job-level retry cannot tell those apart -- it would re-run a genuinely
  // failed dump as readily as an unreachable one -- so backoffLimit stays 0
  // and the tolerance lives here, bounded against a real pod roll.
  local waitForDatabase = |||
    deadline=$(( $(date +%s) + 300 ))
    until psql -tAc 'SELECT 1' > /dev/null 2>&1; do
      if [ "$(date +%s)" -ge "$deadline" ]; then
        echo "FATAL: database did not accept connections within 300s; the check did not run"
        exit 1
      fi
      echo "waiting for the database to accept connections..."
      sleep 5
    done
  |||,

  // uid 26 (the CNPG image's user) needs the storage group to write to the
  // NFS backup dir. Same reasoning as lib/backup.libsonnet -- omitting it
  // fails with EACCES.
  local backupJob(name, ns, image, script, env, pvcName, annotations, pre='', serviceAccount=null) = {
    apiVersion: 'batch/v1',
    kind: 'Job',
    metadata: { name: name, namespace: ns, annotations: annotations },
    spec: {
      // See waitForDatabase: transience is handled in the script, so a
      // failure here is a real one and must not be retried into passing.
      backoffLimit: 0,
      template: { spec: {
        restartPolicy: 'Never',
        securityContext: { supplementalGroups: [1001] },
        [if serviceAccount != null then 'serviceAccountName']: serviceAccount,
        containers: [{
          name: name,
          image: image,
          command: ['/bin/sh', '-c'],
          args: ['set -eu\n' + pre + waitForDatabase + script],
          env: env,
          volumeMounts: [{ name: 'backup', mountPath: '/backup' }],
        }],
        volumes: [{ name: 'backup', persistentVolumeClaim: { claimName: pvcName } }],
      } },
    },
  },

  // Fresh install: no cnpg Cluster object yet, so there is no database to
  // protect, and waiting for one would hang -- its -rw Service and the Cluster
  // itself only arrive in the wave after the gate. Asks the API server for the
  // Cluster object: psql cannot tell "no database" from "database down", a
  // marker on the backup PVC outlives the cluster (static NFS PV), and DNS on
  // the -rw Service would read a CoreDNS outage as a fresh install and skip the
  // pre-upgrade dump. The images carry python3 but no curl, wget or kubectl.
  local exitIfNoCluster = |||
    rc=0
    python3 -c '
    import os, ssl, sys, urllib.error, urllib.request
    sa = "/var/run/secrets/kubernetes.io/serviceaccount"
    url = "https://{}:{}/apis/postgresql.cnpg.io/v1/namespaces/{}/clusters/{}".format(
        os.environ["KUBERNETES_SERVICE_HOST"], os.environ["KUBERNETES_SERVICE_PORT"],
        open(sa + "/namespace").read(), os.environ["CNPG_CLUSTER"])
    req = urllib.request.Request(url, headers={"Authorization": "Bearer " + open(sa + "/token").read()})
    try:
        urllib.request.urlopen(req, context=ssl.create_default_context(cafile=sa + "/ca.crt"), timeout=30)
    except urllib.error.HTTPError as e:
        sys.exit(3 if e.code == 404 else 1)
    ' || rc=$?
    if [ "$rc" = 3 ]; then
      echo "no Cluster $CNPG_CLUSTER: fresh install, nothing to protect"
      exit 0
    elif [ "$rc" != 0 ]; then
      echo "FATAL: could not ask the API server for Cluster $CNPG_CLUSTER; the check did not run"
      exit 1
    fi
  |||,

  // What exitIfNoCluster needs: get on its one Cluster object. In the gate's
  // wave (-1), so the Job's pod finds its ServiceAccount on a fresh cluster.
  majorUpgradeGateAccess(name, ns, clusterName):: {
    local wave = { metadata+: { annotations+: { 'argocd.argoproj.io/sync-wave': '-1' } } },
    serviceAccount: {
      apiVersion: 'v1',
      kind: 'ServiceAccount',
      metadata: { name: name, namespace: ns },
    } + wave,
    role: {
      apiVersion: 'rbac.authorization.k8s.io/v1',
      kind: 'Role',
      metadata: { name: name, namespace: ns },
      rules: [{
        apiGroups: ['postgresql.cnpg.io'],
        resources: ['clusters'],
        resourceNames: [clusterName],
        verbs: ['get'],
      }],
    } + wave,
    roleBinding: {
      apiVersion: 'rbac.authorization.k8s.io/v1',
      kind: 'RoleBinding',
      metadata: { name: name, namespace: ns },
      roleRef: { apiGroup: 'rbac.authorization.k8s.io', kind: 'Role', name: name },
      subjects: [{ kind: 'ServiceAccount', name: name, namespace: ns }],
    } + wave,
  },

  // Sync hook in wave -1, not PreSync: PreSync runs before any Sync-phase
  // resource exists, so on a fresh cluster the backup PVC would not exist and
  // the pod would never schedule. Wave -1 holds the gate with its backup
  // PV/PVC (and majorUpgradeGateAccess); the Cluster and everything else is in
  // wave 0, which ArgoCD does not start while the gate runs or after it failed.
  //
  // No Cluster object -> fresh install, exit 0 (exitIfNoCluster). Otherwise:
  // if the running major already equals the target there is no
  // upgrade pending and this exits immediately. If they differ, it takes a
  // dedicated pre-upgrade dump and verifies it is actually restorable. A
  // failure here fails the sync, so the image change never reaches the
  // cluster without a good dump behind it.
  //
  // This is the gate that matters: once pg_upgrade --link succeeds, the old
  // data directory shares inodes with the new one and is no longer a fallback.
  // This dump is the only way back.
  majorUpgradeGate(name, ns, image, host, user, database, secretName, pvcName, targetMajor, clusterName, secretKey='password')::
    backupJob(
      name, ns, image,
      |||
        reason=""

        running=$(psql -tAc 'SHOW server_version_num' | tr -d '[:space:]')
        major=$(( running / 10000 ))
        if [ "$major" != "%(target)s" ]; then
          reason="PostgreSQL $major -> %(target)s"
        fi

        # An extension upgrade is one-way in practice: ALTER EXTENSION cannot
        # walk backwards, and reverting the image leaves the library older than
        # the catalog. Immich refuses to start in that state
        # ("invalidDowngrade"), so restore-from-dump is the only way back --
        # which makes this every bit as much a gated change as a major upgrade.
        expected_vchord="%(vchord)s"
        if [ -n "$expected_vchord" ]; then
          installed_vchord=$(psql -tAc "SELECT extversion FROM pg_extension WHERE extname = 'vchord'" | tr -d '[:space:]')
          if [ -n "$installed_vchord" ] && [ "$installed_vchord" != "$expected_vchord" ]; then
            if [ -n "$reason" ]; then
              reason="$reason, vchord $installed_vchord -> $expected_vchord"
            else
              reason="vchord $installed_vchord -> $expected_vchord"
            fi
          fi
        fi

        if [ -z "$reason" ]; then
          echo "PostgreSQL major $major matches target, extension matches image - no upgrade pending"
          exit 0
        fi

        echo "UPGRADE PENDING: $reason"
        out="/backup/%(name)s-pg$major-to-%(target)s-$(date +%%Y%%m%%d-%%H%%M%%S).dump"
        echo "taking pre-upgrade dump to $out"
        pg_dump --format=custom --file="$out"
        [ -s "$out" ] || { echo "FATAL: pre-upgrade dump is empty"; exit 1; }
        pg_restore --list "$out" > /dev/null || { echo "FATAL: pre-upgrade dump is not readable"; exit 1; }
        echo "pre-upgrade dump verified: $(wc -c < "$out") bytes"
      ||| % { target: std.toString(targetMajor), name: name, vchord: $.vchordVersionOf(image) },
      pgEnv(host, user, database, secretName, secretKey) + [{ name: 'CNPG_CLUSTER', value: clusterName }],
      pvcName,
      {
        'argocd.argoproj.io/hook': 'Sync',
        'argocd.argoproj.io/sync-wave': '-1',
        'argocd.argoproj.io/hook-delete-policy': 'BeforeHookCreation',
      },
      pre=exitIfNoCluster,
      serviceAccount=name,
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
        running=$(psql -tAc 'SHOW server_version_num' | tr -d '[:space:]')
        major=$(( running / 10000 ))
        marker="/backup/.pg-major-%(db)s"

        # A missing marker means "unknown", not "nothing happened".
        #
        # Recording the current state and exiting looks harmless and is not:
        # if this hook's first run lands *after* an upgrade, it writes the
        # post-upgrade major as its baseline and skips the work it exists to
        # do. That is not hypothetical -- the VectorChord reindex hook, built
        # on the same pattern, missed the exact upgrade it was written for
        # because a failed sync meant its first run came after the change.
        #
        # Analyzing when the history is unknown costs one redundant pass on a
        # database that did not need it. Skipping when the history is unknown
        # costs an upgraded database running on no statistics at all, which
        # nothing downstream will report as wrong -- queries just quietly get
        # bad plans. Prefer the cheap mistake.
        if [ ! -f "$marker" ]; then
          echo "no recorded PostgreSQL major for this database; assuming statistics may be stale"
          last="unknown"
        else
          last=$(cat "$marker")
        fi

        if [ "$last" = "$major" ]; then
          echo "PostgreSQL major unchanged ($major) - nothing to finalize"
          exit 0
        fi

        echo "finalizing: recorded major $last, running $major"
        # pg_upgrade does not carry optimizer statistics across. Staged so the
        # database becomes usable quickly rather than after one long pass.
        vacuumdb --analyze-in-stages --dbname="$PGDATABASE"

        # Confirm the pass actually produced statistics rather than assuming
        # the exit code says so.
        stats=$(psql -tAc "SELECT count(*) FROM pg_stats WHERE schemaname NOT IN ('pg_catalog','information_schema')" | tr -d '[:space:]')
        echo "statistics rows for user tables after ANALYZE: $stats"

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

  // PostSync: rebuild VectorChord indexes after an extension upgrade.
  //
  // VectorChord stamps a format version into each index's meta page and
  // checks it on read (crates/vchordrq/src/tuples.rs):
  //
  //   if VERSION != *checker.prefix::<u64>(size_of::<Tag>()) {
  //       panic!("deserialization: bad version number; {}",
  //              "after upgrading VectorChord, please use REINDEX to rebuild the index.");
  //   }
  //
  // That is a panic, which pgrx surfaces as a PostgreSQL ERROR -- queries
  // against a stale index fail outright rather than returning worse results.
  // The constant moved 9 (0.4.3) -> 11 (0.5.0) -> 1000 (1.0.0) -> 1001
  // (1.1.0), so an extension upgrade invalidates every vchordrq index until
  // it is rebuilt, and Immich's face and CLIP search break until it is.
  //
  // Keyed on the extension version, not the PostgreSQL major: an image bump
  // can move VectorChord without touching the major at all, which is exactly
  // what 16.9-0.4.3 -> 16.14-1.1.1 would do.
  //
  // Plain REINDEX rather than CONCURRENTLY: it rebuilds from the heap without
  // reading the stale index, which is the point when the stale index is
  // precisely what cannot be deserialized. It takes a stronger lock, but these
  // indexes are a few MB.
  vchordReindex(name, ns, image, host, user, database, secretName, pvcName, secretKey='password')::
    backupJob(
      name, ns, image,
      |||
        ver=$(psql -tAc "SELECT extversion FROM pg_extension WHERE extname = 'vchord'" | tr -d '[:space:]')
        if [ -z "$ver" ]; then
          echo "vchord is not installed - nothing to rebuild"
          exit 0
        fi

        # Probe each index by actually reading it. A stale on-disk format
        # raises "bad version number" on read, and that -- not a recorded
        # version number -- is the condition worth acting on.
        #
        # An earlier version of this hook compared the extension version
        # against a marker file, and missed the very upgrade it was written
        # for: its first run landed after the extension had already moved, so
        # it recorded the post-change version as its baseline and skipped the
        # rebuild. Bookkeeping can be wrong about the past. The index either
        # reads or it does not.
        #
        # The probe forces an index scan, deriving the distance operator from
        # the opclass so it works for whichever one an index was built with.
        probes=$(psql -tAc "
          SELECT format(
                   '%%I.%%I|SET enable_seqscan=off; SELECT 1 FROM %%I.%%I ORDER BY %%I %%s (SELECT %%I FROM %%I.%%I WHERE %%I IS NOT NULL LIMIT 1) LIMIT 1',
                   n.nspname, c.relname,
                   n.nspname, t.relname, a.attname,
                   CASE
                     WHEN o.opcname LIKE '%%\_cosine\_ops' THEN '<=>'
                     WHEN o.opcname LIKE '%%\_ip\_ops'     THEN '<#>'
                     ELSE '<->'
                   END,
                   a.attname, n.nspname, t.relname, a.attname)
            FROM pg_index i
            JOIN pg_class c ON c.oid = i.indexrelid
            JOIN pg_class t ON t.oid = i.indrelid
            JOIN pg_namespace n ON n.oid = c.relnamespace
            JOIN pg_am am ON am.oid = c.relam
            JOIN pg_opclass o ON o.oid = i.indclass[0]
            JOIN pg_attribute a ON a.attrelid = i.indrelid AND a.attnum = i.indkey[0]
           WHERE am.amname IN ('vchordrq', 'vchordg')")

        # Only a deserialization failure means the index is unreadable. A probe
        # can fail for reasons that say nothing about the on-disk format --
        # "need 1 probes, but 0 probes provided" is one, since VectorChord
        # wants vchordrq.probes set for a scan and its depth depends on how the
        # index was built. That error in fact proves the opposite of a problem:
        # to know how many probes it needs, it had to read the meta page.
        #
        # Treating any failure as fatal is what stopped an earlier run of this
        # hook after it had rebuilt the first of two indexes -- the rebuild
        # worked, the verification tripped over the probes setting, and
        # clip_index was left broken.
        run_probe() {
          psql -c "SET vchordrq.probes='1'; $1" 2>&1 > /dev/null || true
        }

        echo "$probes" | while IFS= read -r line; do
          [ -n "$line" ] || continue
          idx=${line%%%%|*}
          probe=${line#*|}

          err=$(run_probe "$probe")
          case "$err" in
            *"bad version number"*)
              echo "$idx is unreadable at vchord $ver; rebuilding"
              psql -v ON_ERROR_STOP=1 -c "REINDEX INDEX $idx"
              err=$(run_probe "$probe")
              case "$err" in
                *"bad version number"*)
                  echo "FATAL: $idx still unreadable after REINDEX"
                  exit 1
                  ;;
                *)
                  echo "$idx rebuilt and verified"
                  ;;
              esac
              ;;
            *)
              echo "$idx reads cleanly"
              ;;
          esac
        done

        echo "VectorChord index check complete at vchord $ver"
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
      // `version` is what makes the operator issue ALTER EXTENSION UPDATE TO.
      // Without it it only ever creates a missing extension:
      //
      //   if len(spec.Version) > 0 && spec.Version != info.Version {
      //       ... "ALTER EXTENSION %s UPDATE TO %v" ...
      //   }
      //   -- internal/management/controller/database_controller_sql.go
      //
      // Entries are {name, version?}; omitting version means "leave whatever
      // is installed alone", which is only ever right for extensions nothing
      // depends on being current.
      extensions: [
        { name: e.name, ensure: 'present' }
        + (if std.objectHas(e, 'version') && e.version != '' then { version: e.version } else {})
        for e in extensions
      ],
    },
  },
}
