{
  // Daily pg_dump of a CloudNativePG database to an NFS-backed PVC, keeping the
  // last `keepDays` dumps. Reused by every CNPG-backed service — do not inline.
  //
  // The dump pod runs as the CNPG image's uid (26) and joins the `storage` group
  // (gid 1001) via supplementalGroups so it can write to the group-writable NFS
  // backup dir (root:storage 0775, same as every other NFS dir). That group line
  // is LOAD-BEARING: without it pg_dump fails with EACCES ("could not open output
  // file: Permission denied"), which silently broke Immich's backups for months.
  // Verified at ground truth 2026-07-21.
  pgDumpCronJob(name, ns, image, host, user, database, secretName, pvcName, secretKey='password', keepDays=7):: {
    apiVersion: 'batch/v1',
    kind: 'CronJob',
    metadata: { name: name, namespace: ns },
    spec: {
      schedule: '0 3 * * *',
      concurrencyPolicy: 'Forbid',
      successfulJobsHistoryLimit: 3,
      failedJobsHistoryLimit: 3,
      jobTemplate: { spec: { template: { spec: {
        restartPolicy: 'OnFailure',
        securityContext: { supplementalGroups: [1001] },
        containers: [{
          name: 'pg-dump',
          image: image,
          command: ['/bin/sh', '-c'],
          args: [
            'pg_dump --format=custom --file=/backup/' + name + '-$(date +%Y%m%d-%H%M%S).dump'
            + ' && find /backup -name "*.dump" -mtime +' + keepDays + ' -delete',
          ],
          env: [
            { name: 'PGHOST', value: host },
            { name: 'PGUSER', value: user },
            { name: 'PGDATABASE', value: database },
            { name: 'PGPASSWORD', valueFrom: { secretKeyRef: { name: secretName, key: secretKey } } },
          ],
          volumeMounts: [{ name: 'backup', mountPath: '/backup' }],
        }],
        volumes: [{ name: 'backup', persistentVolumeClaim: { claimName: pvcName } }],
      } } } },
    },
  },

  // Tag half of an image ref, taking the last colon-separated segment so a
  // registry port cannot confuse it.
  local tagOf(image) =
    local parts = std.split(image, ':');
    parts[std.length(parts) - 1],

  // PreSync gate for Forgejo -- the counterpart to the CloudNativePG gate in
  // lib/postgres.libsonnet, for a service that needs the same protection by a
  // different route.
  //
  // Forgejo keeps its database in SQLite on the data PVC, and the deployment
  // runs `forgejo migrate` in an initContainer on *every* pod start. A schema
  // migration therefore fires the moment a new image rolls, with nothing in
  // front of it. Reverting the image does not undo a migration that has
  // already run, so restoring a dump is the only way back -- which makes a
  // fresh, verified dump the precondition for changing the image at all.
  //
  // The nightly dump is not that. It can be almost a day old, and "the backup
  // is probably fine" is exactly the assumption worth removing here.
  //
  // Asks the running instance what version it is, rather than inferring it
  // from anything on disk, and takes a dump only when that differs from the
  // image about to be deployed. On an ordinary sync it is one HTTP request.
  forgejoDumpGate(name, ns, image, service, dataPvc, backupPvc):: {
    apiVersion: 'batch/v1',
    kind: 'Job',
    metadata: {
      name: name,
      namespace: ns,
      annotations: {
        'argocd.argoproj.io/hook': 'PreSync',
        'argocd.argoproj.io/hook-delete-policy': 'BeforeHookCreation',
      },
    },
    spec: {
      // Transience is handled by the wait below, so a failure here is real.
      backoffLimit: 0,
      template: { spec: {
        restartPolicy: 'Never',
        containers: [{
          name: 'dump-gate',
          image: image,
          command: ['/bin/bash', '-c'],
          args: [|||
            set -eu
            expected="%(expected)s"
            url="http://%(service)s:3000/api/v1/version"

            # An ArgoCD retry can land while the pod is rolling from a change
            # an earlier attempt made, so tolerate that rather than failing a
            # sync over a service that is merely restarting. Being unable to
            # ask is different from getting an answer we do not like.
            deadline=$(( $(date +%%s) + 300 ))
            until curl -sf "$url" > /dev/null 2>&1; do
              if [ "$(date +%%s)" -ge "$deadline" ]; then
                echo "FATAL: forgejo did not answer within 300s; the check did not run"
                exit 1
              fi
              echo "waiting for forgejo to answer..."
              sleep 5
            done

            running=$(curl -sf "$url" | sed -n 's/.*"version":"\([0-9][0-9.]*\).*/\1/p')
            if [ -z "$running" ]; then
              echo "FATAL: could not read a version from $url; the check did not run"
              exit 1
            fi

            if [ "$running" = "$expected" ]; then
              echo "forgejo $running already matches the image - no upgrade pending"
              exit 0
            fi

            echo "UPGRADE PENDING: forgejo $running -> $expected"
            out="/backup/forgejo-preupgrade-$running-to-$expected-$(date +%%Y%%m%%d-%%H%%M%%S).tar.gz"
            echo "taking pre-upgrade snapshot of /data to $out"
            # Raw snapshot rather than `forgejo dump`: forgejo's dump is not
            # forward-compatible, so the target binary cannot dump a database
            # that predates it (16.0.3's dump fails on the 16.0.2 schema with
            # "no such column: workflow_source_commit", which only exists after
            # 16.0.3's own migration runs -- and dump does not migrate). The
            # snapshot captures the exact pre-upgrade state at any version and
            # restores by extracting over the data volume.
            tar -czf "$out" -C /data .
            [ -s "$out" ] || { echo "FATAL: pre-upgrade snapshot is empty"; exit 1; }
            tar -tzf "$out" > /dev/null || { echo "FATAL: pre-upgrade snapshot is not readable"; exit 1; }
            echo "pre-upgrade snapshot verified: $(wc -c < "$out") bytes"
          ||| % { expected: tagOf(image), service: service }],
          volumeMounts: [
            { name: 'data', mountPath: '/data' },
            { name: 'backup', mountPath: '/backup' },
          ],
        }],
        volumes: [
          { name: 'data', persistentVolumeClaim: { claimName: dataPvc } },
          { name: 'backup', persistentVolumeClaim: { claimName: backupPvc } },
        ],
      } },
    },
  },
}
