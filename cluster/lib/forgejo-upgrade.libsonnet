// Forgejo upgrades that roll themselves back (FORGEJO_MIGRATION_PLAN.md ->
// Forgejo Upgrades). Forgejo's own procedure -- backup, new version,
// `forgejo doctor check --all` -- as ArgoCD hooks of the Forgejo Application:
//
//   gate      Sync hook, wave -1 (with the PVCs it mounts; the Deployment is in
//             wave 0, which ArgoCD does not start while it runs or after it
//             failed). Fresh install or no version change: nothing to do. An
//             upgrade with the Application locked (automated sync disabled):
//             fail without touching anything. Otherwise write the attempt
//             marker, record which doctor checks fail on the running version
//             (the baseline), scale Forgejo to 0, snapshot /data with it
//             stopped, and pre-flight the new image on a copy of the snapshot
//             (`forgejo migrate`, doctor). Failure: Forgejo back to 1 replica on
//             the old image, sync failed.
//   check     PostSync hook: doctor on the live data and the API answering the
//             new version. Success deletes the marker.
//   syncFail  SyncFail hook: acts only on the marker. A failed pre-flight:
//             Forgejo back on the old image, lock. A failed rollout or check:
//             restore the marker's snapshot, put the old image back, verify
//             it, lock. No marker: the failure was not an upgrade -- nothing,
//             and ArgoCD's retries proceed.
//
// Lock = spec.syncPolicy.automated.enabled: false on the Application, which
// the ApplicationSet ignores (ARGOCD_APPLICATIONS_PLAN.md -> Target). Lifted by
// hand (Restore Runbook -> Forgejo upgrade lock); ForgejoUpgradeLocked alerts
// while it holds.
//
// `doctor check --all` cannot gate anything by its exit status: it exits
// non-zero only when the paths check fails, and returns success when the
// database does not even open (forgejo services/doctor/doctor.go:94-112). So a
// doctor run passes only if it completed, nothing aborted or failed to
// initialize, and every check printing ERROR also failed before -- on the
// running version (the baseline, recorded by the gate) or, on an ordinary
// sync, in the last passing check (/backup/.doctor-baseline). The live
// instance prints ERROR for its LFS garbage collection; that must not block
// upgrades, a new failure must.
{
  local tagOf(image) =
    local parts = std.split(image, ':');
    parts[std.length(parts) - 1],
  local repoOf(image) =
    local parts = std.split(image, ':');
    std.join(':', parts[0:std.length(parts) - 1]),

  // Turns a doctor run on stdin into the titles of its failing checks.
  local errorTitles = |||
    tr -d '\033' | sed 's/\[[0-9;]*m//g' | awk '/^\[[0-9]+\] /{t=$0; sub(/^\[[0-9]+\] /,"",t)} /^ERROR$/{print t}'
  |||,

  // Lines of stdin not in the file. Busybox grep prints nothing for
  // `grep -vxF -f <empty file>` (GNU prints everything), so an empty baseline
  // -- every error new -- must not go through grep.
  local notIn = |||
    not_in() { if [ -s "$1" ]; then grep -vxF -f "$1"; else cat; fi; }
  |||,

  // doctor_ok <config> <baseline file>: the run passes as described above.
  local doctorOk = notIn + |||
    doctor_ok() {
      local out errs new
      out=$(set -o pipefail; su-exec git forgejo doctor check --all --config "$1" 2>&1 | tr -d '\033' | sed 's/\[[0-9;]*m//g') || {
        echo "$out"; echo "doctor: aborted (paths check failed)"; return 1; }
      echo "$out"
      echo "$out" | grep -q '^All done' || { echo "doctor: did not complete"; return 1; }
      if echo "$out" | grep -qE '^FAIL$|Error whilst initializing'; then
        echo "doctor: could not initialize or a check aborted"; return 1
      fi
      errs=$(echo "$out" | %(errorTitles)s)
      new=$(printf '%%s\n' "$errs" | grep -v '^$' | not_in "$2" || true)
      if [ -n "$new" ]; then echo "doctor: now failing, passed before: $new"; return 1; fi
      echo "doctor: no check fails that passed before"
    }
  ||| % { errorTitles: std.strReplace(errorTitles, '\n', '') },

  new(p):: {
    local ns = p.ns,
    local target = tagOf(p.image),
    local marker = '/backup/.upgrade-attempt',
    local url = 'http://%s:3000/api/v1/version' % p.service,
    local lockPatch = '{"spec":{"syncPolicy":{"automated":{"enabled":false}}}}',
    local sa = 'forgejo-upgrade',
    local waveMinus2 = { 'argocd.argoproj.io/sync-wave': '-2' },

    serviceAccount: {
      apiVersion: 'v1',
      kind: 'ServiceAccount',
      metadata: { name: sa, namespace: ns, annotations: waveMinus2 },
    },
    role: {
      apiVersion: 'rbac.authorization.k8s.io/v1',
      kind: 'Role',
      metadata: { name: sa, namespace: ns, annotations: waveMinus2 },
      rules: [
        { apiGroups: ['apps'], resources: ['deployments'], resourceNames: [p.deployment], verbs: ['get', 'patch'] },
        { apiGroups: ['apps'], resources: ['deployments/scale'], resourceNames: [p.deployment], verbs: ['get', 'patch', 'update'] },
        { apiGroups: [''], resources: ['pods'], verbs: ['get', 'list', 'watch'] },
        { apiGroups: [''], resources: ['pods/exec'], verbs: ['create'] },
      ],
    },
    roleBinding: {
      apiVersion: 'rbac.authorization.k8s.io/v1',
      kind: 'RoleBinding',
      metadata: { name: sa, namespace: ns, annotations: waveMinus2 },
      roleRef: { apiGroup: 'rbac.authorization.k8s.io', kind: 'Role', name: sa },
      subjects: [{ kind: 'ServiceAccount', name: sa, namespace: ns }],
    },
    // Read and lock this Forgejo's own Application, nothing else.
    argocdRole: {
      apiVersion: 'rbac.authorization.k8s.io/v1',
      kind: 'Role',
      metadata: { name: sa + '-' + p.app, namespace: 'argocd', annotations: waveMinus2 },
      rules: [
        { apiGroups: ['argoproj.io'], resources: ['applications'], resourceNames: [p.app], verbs: ['get', 'patch'] },
      ],
    },
    argocdRoleBinding: {
      apiVersion: 'rbac.authorization.k8s.io/v1',
      kind: 'RoleBinding',
      metadata: { name: sa + '-' + p.app, namespace: 'argocd', annotations: waveMinus2 },
      roleRef: { apiGroup: 'rbac.authorization.k8s.io', kind: 'Role', name: sa + '-' + p.app },
      subjects: [{ kind: 'ServiceAccount', name: sa, namespace: ns }],
    },

    local hookMeta(name, hook, wave=null) = {
      name: name,
      namespace: ns,
      annotations: {
        'argocd.argoproj.io/hook': hook,
        'argocd.argoproj.io/hook-delete-policy': 'BeforeHookCreation',
      } + (if wave != null then { 'argocd.argoproj.io/sync-wave': wave } else {}),
    },
    local vol(name, claim) = { name: name, persistentVolumeClaim: { claimName: claim } },
    local mounts(list) = [{ name: m[0], mountPath: m[1] } for m in list],

    gate: {
      apiVersion: 'batch/v1',
      kind: 'Job',
      metadata: hookMeta(p.name + '-gate', 'Sync', '-1'),
      spec: {
        backoffLimit: 0,
        template: { spec: {
          restartPolicy: 'Never',
          serviceAccountName: sa,
          initContainers: [
            {
              name: 'prepare',
              image: p.controlImage,
              command: ['/bin/bash', '-c'],
              args: [|||
                set -eu
                echo skip > /work/decision
                if [ ! -f /data/gitea/gitea.db ]; then
                  echo "no /data/gitea/gitea.db: fresh install, nothing to protect"
                  exit 0
                fi
                # Tolerate a pod still rolling from an earlier attempt; being
                # unable to ask is not an answer.
                deadline=$(( $(date +%%s) + 300 ))
                until curl -sf %(url)s >/dev/null 2>&1; do
                  [ "$(date +%%s)" -lt "$deadline" ] || { echo "FATAL: forgejo did not answer within 300s"; exit 1; }
                  echo "waiting for forgejo to answer..."; sleep 5
                done
                from=$(curl -sf %(url)s | jq -r .version)
                if [ "$from" = "%(target)s" ]; then
                  echo "forgejo $from matches the image: no upgrade pending"
                  exit 0
                fi
                enabled=$(kubectl get application -n argocd %(app)s -o jsonpath='{.spec.syncPolicy.automated.enabled}')
                if [ "$enabled" = "false" ]; then
                  echo "LOCKED: upgrade $from -> %(target)s is locked (automated sync disabled after a failed upgrade); see the Restore Runbook -> Forgejo upgrade lock"
                  exit 1
                fi
                # The baseline: which doctor checks fail on the running version.
                # Before the marker, so a failure here locks nothing.
                set -o pipefail
                kubectl exec -n %(ns)s deploy/%(deployment)s -c %(container)s -- \
                  su-exec git forgejo doctor check --all --config /data/gitea/conf/app.ini 2>&1 \
                  | %(errorTitles)s > /work/baseline
                echo "baseline (checks failing on $from): $(tr '\n' ';' < /work/baseline)"

                snap=/backup/forgejo-preupgrade-$from-to-%(target)s-$(date +%%Y%%m%%d-%%H%%M%%S).tar.gz
                cp /work/baseline /backup/.upgrade-baseline
                echo "$from %(target)s $snap preflight" > %(marker)s
                echo "$from" > /work/from
                echo "UPGRADE: forgejo $from -> %(target)s; marker written"

                # Stopped, so the snapshot is a point in time, not a live SQLite.
                kubectl scale -n %(ns)s deploy/%(deployment)s --replicas=0
                trap 'echo "gate failed after scale-down: restoring 1 replica"; kubectl scale -n %(ns)s deploy/%(deployment)s --replicas=1' ERR
                kubectl wait -n %(ns)s --for=delete pod -l %(selector)s --timeout=300s
                tar -czf "$snap" -C /data .
                tar -tzf "$snap" >/dev/null
                echo "snapshot $snap: $(wc -c < "$snap") bytes, readable"
                tar -xzf "$snap" -C /scratch
                echo preflight > /work/decision
              ||| % {
                url: url,
                target: target,
                app: p.app,
                marker: marker,
                ns: ns,
                deployment: p.deployment,
                container: p.container,
                selector: p.podSelector,
                errorTitles: std.strReplace(errorTitles, '\n', ''),
              }],
              volumeMounts: mounts([['data', '/data'], ['backup', '/backup'], ['work', '/work'], ['scratch', '/scratch']]),
            },
            {
              // The new image on a copy of the snapshot; live data untouched.
              name: 'preflight',
              image: p.image,
              command: ['/bin/bash', '-c'],
              args: [|||
                set -u
                [ "$(cat /work/decision)" = preflight ] || exit 0
                %(doctorOk)s
                if su-exec git forgejo migrate --config /data/gitea/conf/app.ini \
                   && doctor_ok /data/gitea/conf/app.ini /backup/.upgrade-baseline; then
                  echo ok > /work/preflight
                else
                  echo failed > /work/preflight
                fi
              ||| % { doctorOk: doctorOk }],
              volumeMounts: mounts([['scratch', '/data'], ['backup', '/backup'], ['work', '/work']]),
            },
          ],
          containers: [{
            name: 'decide',
            image: p.controlImage,
            command: ['/bin/bash', '-c'],
            args: [|||
              set -eu
              [ "$(cat /work/decision)" = preflight ] || exit 0
              if [ "$(cat /work/preflight)" != ok ]; then
                kubectl scale -n %(ns)s deploy/%(deployment)s --replicas=1
                echo "PRE-FLIGHT FAILED: forgejo $(cat /work/from) -> %(target)s; the old version keeps serving"
                exit 1
              fi
              sed -i 's/ preflight$/ rollout/' %(marker)s
              echo "pre-flight passed: rolling out %(target)s"
            ||| % { ns: ns, deployment: p.deployment, target: target, marker: marker }],
            volumeMounts: mounts([['backup', '/backup'], ['work', '/work']]),
          }],
          volumes: [
            vol('data', p.dataPvc),
            vol('backup', p.backupPvc),
            { name: 'work', emptyDir: {} },
            { name: 'scratch', emptyDir: {} },
          ],
        } },
      },
    },

    check: {
      apiVersion: 'batch/v1',
      kind: 'Job',
      metadata: hookMeta(p.name + '-check', 'PostSync'),
      spec: {
        backoffLimit: 0,
        template: { spec: {
          restartPolicy: 'Never',
          containers: [{
            name: 'check',
            image: p.image,
            command: ['/bin/bash', '-c'],
            args: [|||
              set -u
              %(doctorOk)s
              baseline=/backup/.doctor-baseline
              if [ -e %(marker)s ]; then
                baseline=/backup/.upgrade-baseline
              elif [ ! -e "$baseline" ]; then
                # First check on this instance, no upgrade in flight: what fails
                # now is the baseline later runs are held to.
                su-exec git forgejo doctor check --all --config /data/gitea/conf/app.ini 2>&1 \
                  | %(errorTitles)s > "$baseline"
                echo "first check: baseline is $(tr '\n' ';' < "$baseline")"
              fi
              version=$(curl -sf %(url)s | sed -n 's/.*"version":"\([^"]*\)".*/\1/p')
              if [ "$version" != "%(target)s" ]; then
                echo "CHECK FAILED: forgejo answers '$version', expected %(target)s"; exit 1
              fi
              doctor_ok /data/gitea/conf/app.ini "$baseline" || { echo "CHECK FAILED: doctor"; exit 1; }
              %(forceFail)s
              su-exec git forgejo doctor check --all --config /data/gitea/conf/app.ini 2>&1 \
                | %(errorTitles)s > /backup/.doctor-baseline
              rm -f %(marker)s /backup/.upgrade-baseline
              echo "forgejo %(target)s passes; upgrade (if any) complete"
            ||| % {
              doctorOk: doctorOk,
              marker: marker,
              url: url,
              target: target,
              errorTitles: std.strReplace(errorTitles, '\n', ''),
              forceFail: if std.get(p, 'failCheckForTest', false)
              then 'echo "CHECK FAILED: forced for the upgrade test (failCheckForTest)"; exit 1'
              else '',
            }],
            volumeMounts: mounts([['data', '/data'], ['backup', '/backup']]),
          }],
          volumes: [vol('data', p.dataPvc), vol('backup', p.backupPvc)],
        } },
      },
    },

    syncFail: {
      apiVersion: 'batch/v1',
      kind: 'Job',
      metadata: hookMeta(p.name + '-syncfail', 'SyncFail'),
      spec: {
        backoffLimit: 0,
        template: { spec: {
          restartPolicy: 'Never',
          serviceAccountName: sa,
          containers: [{
            name: 'rollback',
            image: p.controlImage,
            command: ['/bin/bash', '-c'],
            args: [|||
              set -eu
              %(notIn)s
              if [ ! -e %(marker)s ]; then
                echo "no upgrade in flight: not a failed upgrade, nothing to undo"
                exit 0
              fi
              read -r from to snap stage < %(marker)s
              old=%(repo)s:$from
              echo "failed upgrade $from -> $to at stage $stage"
              # Lock first: selfHeal must not undo what follows, and every retry
              # must stop at the gate.
              kubectl patch application -n argocd %(app)s --type merge -p '%(lockPatch)s'
              echo "locked: automated sync disabled on %(app)s"
              case "$stage" in
                preflight)
                  # The new image never rolled; make sure the old one serves.
                  kubectl scale -n %(ns)s deploy/%(deployment)s --replicas=1
                  ;;
                rollout)
                  running=$(curl -sf %(url)s | jq -r .version || true)
                  if [ "$running" != "$from" ]; then
                    kubectl scale -n %(ns)s deploy/%(deployment)s --replicas=0
                    kubectl wait -n %(ns)s --for=delete pod -l %(selector)s --timeout=300s
                    find /data -mindepth 1 -maxdepth 1 -exec rm -rf {} +
                    tar -xzf "$snap" -C /data
                    echo "restored $snap into the data volume"
                    kubectl set image -n %(ns)s deploy/%(deployment)s %(container)s="$old" %(initContainer)s="$old"
                    kubectl scale -n %(ns)s deploy/%(deployment)s --replicas=1
                  fi
                  deadline=$(( $(date +%%s) + 600 ))
                  until [ "$(curl -sf %(url)s | jq -r .version 2>/dev/null)" = "$from" ]; do
                    [ "$(date +%%s)" -lt "$deadline" ] || { echo "FATAL: forgejo did not come back as $from"; exit 1; }
                    sleep 5
                  done
                  kubectl exec -n %(ns)s deploy/%(deployment)s -c %(container)s -- \
                    su-exec git forgejo doctor check --all --config /data/gitea/conf/app.ini 2>&1 \
                    | %(errorTitles)s > /tmp/now
                  if not_in /backup/.upgrade-baseline < /tmp/now | grep -q .; then
                    echo "FATAL: restored $from fails doctor checks it passed before: $(not_in /backup/.upgrade-baseline < /tmp/now | tr '\n' ';')"
                    exit 1
                  fi
                  echo "forgejo $from restored and passing"
                  ;;
              esac
              rm -f %(marker)s
              echo "ROLLED BACK: upgrade $from -> $to failed at $stage; forgejo serves $from, %(app)s locked until lifted by hand"
              exit 1
            ||| % {
              notIn: notIn,
              marker: marker,
              repo: repoOf(p.image),
              app: p.app,
              lockPatch: lockPatch,
              ns: ns,
              deployment: p.deployment,
              url: url,
              selector: p.podSelector,
              container: p.container,
              initContainer: p.initContainer,
              errorTitles: std.strReplace(errorTitles, '\n', ''),
            }],
            volumeMounts: mounts([['data', '/data'], ['backup', '/backup']]),
          }],
          volumes: [vol('data', p.dataPvc), vol('backup', p.backupPvc)],
        } },
      },
    },
  },
}
