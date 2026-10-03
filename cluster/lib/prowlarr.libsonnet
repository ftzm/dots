local k = import 'k8s-libsonnet/main.libsonnet';

{
  // Disable Prowlarr indexers that keep failing, and re-enable them when
  // they recover.
  //
  // Public indexers die, move behind Cloudflare, or start returning errors;
  // Prowlarr's own failure backoff only skips them for a while, and every
  // search still waits on whichever are slow to fail (FlareSolverr timeouts
  // plus Prowlarr's two retries cost minutes per search). This adds a
  // sidecar that, every `interval` seconds, runs Prowlarr's own indexer test
  // (one RSS page; passes when it returns releases) on each indexer, one at
  // a time:
  //   - an enabled indexer whose test has failed without a pass for at least
  //     `threshold` seconds is disabled;
  //   - an indexer this pruner disabled is re-enabled on its first pass.
  // Prowlarr syncs the enable flag to Sonarr/Radarr, so a pruned indexer
  // stops being searched there too. Indexers disabled by hand are left alone.
  //
  // Only a 400 from the test endpoint (Prowlarr's validation failure) counts
  // as a failed test; any other outcome (Prowlarr down, timeout) leaves the
  // indexer's record unchanged. State — first failure time and whether the
  // pruner disabled it — is kept per indexer id in /config/indexer-pruner.json
  // so it survives restarts.
  //
  // Usage (the sidecar reuses the Prowlarr image, which ships curl and jq):
  //   local pruner = prowlarr.pruneFailingIndexers(images.prowlarr);
  //   selfhosted.new('prowlarr', ...) { indexerPrunerScript: pruner.configMap, deployment+: pruner.deploymentMixin }
  //
  pruneFailingIndexers(image, port=9696, ns='media', interval=21600, threshold=172800):: {
    local cmName = 'prowlarr-prune-failing-indexers',

    local script = |||
      #!/bin/sh
      set -u
      api="http://localhost:%(port)d/api/v1"
      state=/config/indexer-pruner.json
      threshold=%(threshold)d

      # Set an indexer's enable flag. forceSave skips the connectivity
      # test; masked secrets ("********") are restored server-side.
      set_enable() {
        printf '%%s' "$1" | jq -c --argjson on "$2" '.enable = $on' |
          curl -fsS -o /dev/null -m 120 -X PUT \
            -H "X-Api-Key: $key" -H 'Content-Type: application/json' \
            --data-binary @- "$api/indexer/$(printf '%%s' "$1" | jq -r .id)?forceSave=true"
      }

      while :; do
        # Retry soon unless a full pass completes (Prowlarr may still be
        # starting when the sidecar does).
        pause=60
        key=$(sed -n 's:.*<ApiKey>\(.*\)</ApiKey>.*:\1:p' /config/config.xml 2>/dev/null)
        if [ -z "$key" ]; then
          echo "no API key in /config/config.xml yet"
        elif ! indexers=$(curl -fsS -m 120 -H "X-Api-Key: $key" "$api/indexer"); then
          echo "failed to list indexers"
        else
          old=$(jq -c . "$state" 2>/dev/null || echo '{}')
          new='{}'
          for id in $(printf '%%s' "$indexers" | jq -r '.[].id'); do
            indexer=$(printf '%%s' "$indexers" | jq -c --argjson id "$id" '.[] | select(.id == $id)')
            label=$(printf '%%s' "$indexer" | jq -r .name)
            enabled=$(printf '%%s' "$indexer" | jq -r .enable)
            entry=$(printf '%%s' "$old" | jq -c --arg id "$id" '.[$id] // {}')
            # "pruned" only means something while the indexer is still
            # disabled; once someone re-enables it, it is a normal indexer.
            pruned=$(printf '%%s' "$entry" | jq -r --argjson on "$enabled" '(.pruned // false) and ($on | not)')
            if [ "$enabled" = false ] && [ "$pruned" = false ]; then
              continue # disabled by hand: not ours to manage
            fi

            code=$(printf '%%s' "$indexer" | curl -sS -o /tmp/test-result -w '%%{http_code}' -m 900 -X POST \
              -H "X-Api-Key: $key" -H 'Content-Type: application/json' \
              --data-binary @- "$api/indexer/test?forceTest=true")
            now=$(date +%%s)
            case "$code" in
              2??)
                echo "indexer $id ($label) test passed"
                entry='{}'
                if [ "$pruned" = true ]; then
                  if set_enable "$indexer" true; then
                    echo "re-enabled indexer $id ($label): test passes again"
                  else
                    echo "FAILED to re-enable indexer $id ($label)"
                    entry='{"pruned":true}'
                  fi
                fi
                ;;
              400)
                reason=$(jq -r 'map(.errorMessage) | join("; ")' /tmp/test-result 2>/dev/null)
                first=$(printf '%%s' "$entry" | jq -r --argjson now "$now" '.firstFailure // $now')
                echo "indexer $id ($label) test failed (failing since $(date -d "@$first" -Iseconds)): $reason"
                if [ "$enabled" = true ] && [ $((now - first)) -ge "$threshold" ]; then
                  if set_enable "$indexer" false; then
                    echo "disabled indexer $id ($label): failing for $(((now - first) / 3600))h"
                    pruned=true
                  else
                    echo "FAILED to disable indexer $id ($label)"
                  fi
                fi
                entry=$(jq -nc --argjson first "$first" --argjson pruned "$pruned" '{firstFailure: $first, pruned: $pruned}')
                ;;
              *)
                echo "indexer $id ($label) test inconclusive (HTTP $code); record unchanged"
                ;;
            esac
            if [ "$entry" != '{}' ]; then
              new=$(printf '%%s' "$new" | jq -c --arg id "$id" --argjson e "$entry" '.[$id] = $e')
            fi
          done
          printf '%%s\n' "$new" > "$state.tmp" && mv "$state.tmp" "$state"
          pause=%(interval)d
        fi
        sleep "$pause"
      done
    ||| % { port: port, interval: interval, threshold: threshold },

    configMap: k.core.v1.configMap.new(cmName)
      + k.core.v1.configMap.metadata.withNamespace(ns)
      + k.core.v1.configMap.withData({ 'reconcile.sh': script }),

    deploymentMixin:
      // The script runs once per container start, so roll the pod when it changes.
      k.apps.v1.deployment.spec.template.metadata.withAnnotationsMixin({
        'checksum/prune-failing-indexers': std.md5(script),
      })
      + k.apps.v1.deployment.spec.template.spec.withVolumesMixin([
        k.core.v1.volume.fromConfigMap('prune-failing-indexers', cmName)
        + k.core.v1.volume.configMap.withDefaultMode(std.parseOctal('0555')),
      ])
      + k.apps.v1.deployment.spec.template.spec.withContainersMixin([
        k.core.v1.container.new('prune-failing-indexers', image)
        + k.core.v1.container.withCommand(['/bin/sh', '/scripts/reconcile.sh'])
        + k.core.v1.container.withVolumeMounts([
          // Read-write: the pruner keeps its state file next to Prowlarr's.
          k.core.v1.volumeMount.new('config', '/config'),
          k.core.v1.volumeMount.new('prune-failing-indexers', '/scripts') + k.core.v1.volumeMount.withReadOnly(true),
        ]),
      ]),
  },
}
