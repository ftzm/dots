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
}
