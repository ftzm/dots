local selfhosted = import 'selfhosted.libsonnet';
local k = import 'k8s-libsonnet/main.libsonnet';

{
  // Cleanuparr (https://github.com/Cleanuparr/Cleanuparr) with its settings
  // declared in jsonnet.
  //
  // Cleanuparr reads only infrastructure settings (PORT, PUID, database) from
  // the environment; everything else — arr instances, download clients,
  // malware blocker, queue cleaner, the admin account — lives in its own
  // database and is set through its REST API. So this renders, next to the
  // app, a `cleanuparr-configure` CronJob that applies `settings` through
  // that API every 10 minutes: it completes first-run setup (admin account)
  // if needed, upserts the declared arr instances and download clients by
  // name, deletes undeclared ones, and PUTs the declared job configs. Edits
  // made in the UI to anything declared here are reverted on the next run.
  //
  // Secrets come from the Secret `secretName`, mounted as env vars:
  //   CLEANUPARR_PASSWORD  admin password (username: admin)
  //   plus whatever `apiKeyEnv` / `passwordEnv` names `settings` references.
  //
  // `blocklists` is {filename: [pattern, ...]}; each list is mounted at
  // /blocklists/<filename> for malware-blocker `blocklistPath`s.
  //
  // settings: {
  //   arrs: { sonarr: [{ name, url, version, apiKeyEnv }], radarr: [...] },
  //   downloadClients: [{ name, typeName, type, host, username?, passwordEnv }],
  //   malwareBlocker: <PUT /api/configuration/malware_blocker body>,
  //   queueCleaner: <PUT /api/configuration/queue_cleaner body>,
  //   general: <fields merged into GET /api/configuration/general, then PUT>,
  //   seeker: <fields merged into GET /api/configuration/seeker, then PUT>,
  // }
  //
  // `toolsImage` runs the CronJob and needs sh, curl and jq (the Cleanuparr
  // image has no jq).
  new(image, toolsImage, domain, ns, secretName, settings, blocklists={}):: {
    local port = 11011,
    local app = selfhosted.new('cleanuparr', image, port, domain, ns=ns),

    configPvc: app.configPvc,
    service: app.service,
    ingressRoute: app.ingressRoute,
    deployment: app.deployment
      + k.apps.v1.deployment.spec.template.spec.withVolumesMixin([
        k.core.v1.volume.fromConfigMap('blocklists', 'cleanuparr-blocklists'),
      ])
      + {
        spec+: { template+: { spec+: { containers: [
          super.containers[0]
          + k.core.v1.container.withVolumeMountsMixin([
            k.core.v1.volumeMount.new('blocklists', '/blocklists') + k.core.v1.volumeMount.withReadOnly(true),
          ])
          + k.core.v1.container.readinessProbe.httpGet.withPath('/health')
          + k.core.v1.container.readinessProbe.httpGet.withPort(port),
        ] } } },
      },

    blocklists: k.core.v1.configMap.new('cleanuparr-blocklists')
      + k.core.v1.configMap.metadata.withNamespace(ns)
      + k.core.v1.configMap.withData({
        [name]: std.join('\n', blocklists[name]) + '\n'
        for name in std.objectFields(blocklists)
      }),

    configureScript: k.core.v1.configMap.new('cleanuparr-configure')
      + k.core.v1.configMap.metadata.withNamespace(ns)
      + k.core.v1.configMap.withData({
        'settings.json': std.manifestJsonEx(settings, '  '),
        'configure.sh': importstr 'cleanuparr-configure.sh',
      }),

    configureCronJob: {
      apiVersion: 'batch/v1',
      kind: 'CronJob',
      metadata: { name: 'cleanuparr-configure', namespace: ns },
      spec: {
        schedule: '*/10 * * * *',
        concurrencyPolicy: 'Forbid',
        successfulJobsHistoryLimit: 1,
        failedJobsHistoryLimit: 3,
        jobTemplate: { spec: {
          backoffLimit: 0,
          template: { spec: {
            restartPolicy: 'Never',
            containers: [{
              name: 'configure',
              image: toolsImage,
              command: ['/bin/sh', '/scripts/configure.sh'],
              env: [
                { name: 'CLEANUPARR_URL', value: 'http://cleanuparr.%s.svc.cluster.local:%d' % [ns, port] },
                { name: 'SETTINGS', value: '/scripts/settings.json' },
              ],
              envFrom: [{ secretRef: { name: secretName } }],
              volumeMounts: [{ name: 'scripts', mountPath: '/scripts', readOnly: true }],
            }],
            volumes: [{ name: 'scripts', configMap: { name: 'cleanuparr-configure' } }],
          } },
        } },
      },
    },
  },
}
