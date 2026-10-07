// The Forgejo OpenTofu job (FORGEJO_MIGRATION_PLAN.md -> Secrets -> Forgejo
// configuration as OpenTofu), and the runner monitoring it provisions.
//
// The config (environments/lab/forgejo-tofu/main.tf and its
// .terraform.lock.hcl) is an ordinary ConfigMap of the Forgejo Application,
// so editing it makes the app OutOfSync; the Job is a PostSync hook, so it
// runs once the sync's resources are Healthy (Forgejo's API is up) and only
// when that Application syncs. Its image is nix-built
// (pkgs/forgejo-tofu-image.nix) and imported into k3s on nuc
// (role/k3s-local-images.nix): `imagePullPolicy: Never`, pinned to nuc.
// Providers download at `tofu init`, verified against the lock file.
{
  new(p):: {
    local ns = p.ns,
    local name = 'forgejo-tofu',
    local labels = { 'app.kubernetes.io/name': name },

    config: {
      apiVersion: 'v1',
      kind: 'ConfigMap',
      metadata: { name: name, namespace: ns },
      data: {
        'main.tf': p.mainTf,
        '.terraform.lock.hcl': p.lockFile,
      },
    },

    // The kubernetes backend keeps state in a Secret and locks with a Lease;
    // the config writes the monitor token Secret.
    serviceAccount: {
      apiVersion: 'v1',
      kind: 'ServiceAccount',
      metadata: { name: name, namespace: ns },
    },
    role: {
      apiVersion: 'rbac.authorization.k8s.io/v1',
      kind: 'Role',
      metadata: { name: name, namespace: ns },
      rules: [
        { apiGroups: [''], resources: ['secrets'], verbs: ['get', 'list', 'watch', 'create', 'update', 'patch', 'delete'] },
        { apiGroups: ['coordination.k8s.io'], resources: ['leases'], verbs: ['get', 'list', 'watch', 'create', 'update', 'patch', 'delete'] },
      ],
    },
    roleBinding: {
      apiVersion: 'rbac.authorization.k8s.io/v1',
      kind: 'RoleBinding',
      metadata: { name: name, namespace: ns },
      roleRef: { apiGroup: 'rbac.authorization.k8s.io', kind: 'Role', name: name },
      subjects: [{ kind: 'ServiceAccount', name: name, namespace: ns }],
    },

    job: {
      apiVersion: 'batch/v1',
      kind: 'Job',
      metadata: {
        name: name,
        namespace: ns,
        annotations: {
          'argocd.argoproj.io/hook': 'PostSync',
          'argocd.argoproj.io/hook-delete-policy': 'BeforeHookCreation',
          // After the upgrade check (PostSync, wave 0): configure only a
          // Forgejo that passed it.
          'argocd.argoproj.io/sync-wave': '1',
        },
      },
      spec: {
        backoffLimit: 1,
        template: {
          metadata: { labels: labels },
          spec: {
            restartPolicy: 'Never',
            serviceAccountName: name,
            nodeSelector: { 'kubernetes.io/hostname': 'nuc' },
            containers: [{
              name: 'tofu',
              image: p.image,
              imagePullPolicy: 'Never',
              command: ['/bin/bash', '-c'],
              args: [|||
                set -euo pipefail
                mkdir -p /tmp/work && cp /config/main.tf /config/.terraform.lock.hcl /tmp/work/
                cd /tmp/work
                tofu init -input=false -no-color -lockfile=readonly
                tofu apply -input=false -no-color -auto-approve

                # Push mirrors (the provider has none): created if absent,
                # with an SSH key Forgejo generates; its public half goes into
                # the target's write keys (nas: role/git-mirror.nix).
                api() { curl -sf -u "$FORGEJO_USERNAME:$FORGEJO_PASSWORD" -H 'Content-Type: application/json' "$@"; }
                echo "$PUSH_MIRRORS" | jq -c '.[]' | while read -r m; do
                  repo=$(jq -r .repo <<<"$m"); url=$(jq -r .remote_address <<<"$m")
                  existing=$(api "$FORGEJO_HOST/api/v1/repos/$repo/push_mirrors" | jq -c --arg u "$url" '.[] | select(.remote_address == $u)')
                  if [ -z "$existing" ]; then
                    existing=$(jq 'del(.repo)' <<<"$m" | api -X POST --data @- "$FORGEJO_HOST/api/v1/repos/$repo/push_mirrors")
                    echo "created push mirror $repo -> $url"
                  fi
                  echo "push mirror $repo -> $url: public key $(jq -r .public_key <<<"$existing")"
                done
              |||],
              env: [
                { name: 'FORGEJO_HOST', value: p.forgejoUrl },
                { name: 'PUSH_MIRRORS', value: std.manifestJsonMinified(p.pushMirrors) },
                { name: 'FORGEJO_USERNAME', valueFrom: { secretKeyRef: { name: 'forgejo-secrets', key: 'admin-username' } } },
                { name: 'FORGEJO_PASSWORD', valueFrom: { secretKeyRef: { name: 'forgejo-secrets', key: 'admin-password' } } },
              ],
              volumeMounts: [{ name: 'config', mountPath: '/config', readOnly: true }],
            }],
            volumes: [{ name: 'config', configMap: { name: name } }],
          },
        },
      },
    },

    // --- Runner monitoring ----------------------------------------------
    // Forgejo's metrics have no runner state. json_exporter reads
    // GET /admin/actions/runners on each scrape with the monitor bot's
    // read:admin token (written by the config above) and exposes
    // forgejo_runner_info{runner, status}; Forgejo's statuses are offline,
    // idle and active.
    local monitor = 'forgejo-runner-monitor',
    local monitorLabels = { 'app.kubernetes.io/name': monitor },
    local runnersUrl = p.forgejoUrl + '/api/v1/admin/actions/runners',

    monitorConfig: {
      apiVersion: 'v1',
      kind: 'ConfigMap',
      metadata: { name: monitor, namespace: ns },
      data: {
        'config.yml': std.manifestYamlDoc({
          modules: {
            runners: {
              http_client_config: {
                authorization: { type: 'token', credentials_file: '/token/token' },
              },
              metrics: [{
                name: 'forgejo_runner',
                type: 'object',
                help: 'A runner registered with Forgejo, by status',
                path: '{[*]}',
                labels: { runner: '{.name}', status: '{.status}' },
                values: { info: 1 },
              }],
            },
            // A push mirror's last error ('' when its last push worked).
            push_mirrors: {
              http_client_config: {
                authorization: { type: 'token', credentials_file: '/token/token' },
              },
              metrics: [{
                name: 'forgejo_push_mirror',
                type: 'object',
                help: 'A push mirror, with its last error',
                path: '{[*]}',
                labels: { repo: '{.repo_name}', remote: '{.remote_address}', last_error: '{.last_error}' },
                values: { info: 1 },
              }],
            },
          },
        }),
      },
    },
    monitorDeployment: {
      apiVersion: 'apps/v1',
      kind: 'Deployment',
      metadata: { name: monitor, namespace: ns, labels: monitorLabels },
      spec: {
        replicas: 1,
        selector: { matchLabels: monitorLabels },
        template: {
          metadata: { labels: monitorLabels },
          spec: {
            containers: [{
              name: 'json-exporter',
              image: p.jsonExporterImage,
              args: ['--config.file=/config/config.yml'],
              ports: [{ name: 'http', containerPort: 7979 }],
              volumeMounts: [
                { name: 'config', mountPath: '/config', readOnly: true },
                { name: 'token', mountPath: '/token', readOnly: true },
              ],
            }],
            volumes: [
              { name: 'config', configMap: { name: monitor } },
              // Created by the OpenTofu job; until it exists the probe fails
              // and ForgejoRunnerMonitorDown says so.
              { name: 'token', secret: { secretName: 'forgejo-monitor-token', optional: true } },
            ],
          },
        },
      },
    },
    monitorService: {
      apiVersion: 'v1',
      kind: 'Service',
      metadata: { name: monitor, namespace: ns, labels: monitorLabels },
      spec: {
        selector: monitorLabels,
        ports: [{ name: 'http', port: 7979, targetPort: 'http' }],
      },
    },
    monitorServiceMonitor: {
      apiVersion: 'monitoring.coreos.com/v1',
      kind: 'ServiceMonitor',
      metadata: { name: monitor, namespace: ns },
      spec: {
        selector: { matchLabels: monitorLabels },
        endpoints: [{
          port: 'http',
          path: '/probe',
          params: { module: ['runners'], target: [runnersUrl] },
          interval: '60s',
        }] + [{
          port: 'http',
          path: '/probe',
          params: { module: ['push_mirrors'], target: ['%s/api/v1/repos/%s/push_mirrors' % [p.forgejoUrl, m.repo]] },
          interval: '60s',
        } for m in p.pushMirrors],
      },
    },
    monitorRules: p.alerts.prometheusRule('forgejo-runners', ns, [
      p.alerts.rule(
        'ForgejoRunnerOffline',
        'forgejo_runner_info{status="offline"} == 1',
        '15m', 'warning',
        'Forgejo runner {{ $labels.runner }} offline',
        'Forgejo reports the runner {{ $labels.runner }} offline for 15 minutes, so no Actions job that needs it runs. Check its host (the nuc-microvm runner: `systemctl status microvm@*` on nuc) and its logs.',
      ),
      // The nas mirror refuses a push that would rewind master
      // (receive.denyNonFastForwards): a stale Forgejo -- a restored dump, a
      // force-push -- shows here (FORGEJO_MIGRATION_PLAN.md -> Decisions).
      p.alerts.rule(
        'ForgejoPushMirrorFailing',
        'forgejo_push_mirror_info{last_error!=""} == 1',
        '10m', 'warning',
        'Push mirror {{ $labels.repo }} -> {{ $labels.remote }} failing',
        'Forgejo\'s push mirror of {{ $labels.repo }} to {{ $labels.remote }} reports: {{ $labels.last_error }}. If nas refused a non-fast-forward push, Forgejo is behind or rewound relative to the nas mirror (the deploy source): bring Forgejo\'s master to nas\'s and the next push succeeds.',
      ),
      p.alerts.rule(
        'ForgejoRunnerMonitorDown',
        'absent(forgejo_runner_info)',
        '15m', 'warning',
        'Forgejo runner state unknown',
        'No forgejo_runner_info for 15 minutes: json_exporter (forgejo-runner-monitor) cannot read /admin/actions/runners, or Forgejo lists no runners. Check the exporter\'s logs and the forgejo-monitor-token Secret, which the forgejo-tofu Job writes.',
      ),
    ]),
  },
}
