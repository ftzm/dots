# The deploy plane on nuc (FORGEJO_MIGRATION_PLAN.md -> Binary Cache):
#
# - fleet-write: turns master into a manifest. Every minute it asks the deploy
#   source (the nas mirror; GitHub until the flip) for master's head; when it
#   moved, it builds the flake's `fleet` linkFarm at that exact commit (a
#   no-op when CI built it pre-merge), and if the set of systems changed,
#   advances two profiles -- the linkFarm and the manifest, each keeping two
#   generations as GC roots -- and points /var/www/fleet/manifest at the new
#   manifest's store path. A commit that changes no system (a README) moves
#   only the writer's own stamp; no host does anything. Rollback is a revert
#   commit; the writer has no way to pin anything but master.
# - harmonia: serves nuc's store, signing what it serves with the cache key,
#   the one key hosts trust -- the manifest is a store path verified the same
#   way.
# - nginx: the one-line pointer, /fleet/manifest, on :5001 of every address
#   (traefik holds :80/:443).
#
# Writer metrics, via node_exporter's textfile dir:
# fleet_writer_last_success_timestamp, fleet_writer_last_failure,
# fleet_manifest_commit_info.
{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.fleetWriter;
  stateDir = "/var/lib/fleet-writer";
  profileDir = "/nix/var/nix/profiles/per-user/fleet-writer";
  wwwDir = "/var/www/fleet";

  writer = pkgs.writeShellApplication {
    name = "fleet-write";
    runtimeInputs = [config.nix.package pkgs.git pkgs.openssh pkgs.jq pkgs.coreutils];
    text = ''
      repo=${lib.escapeShellArg cfg.repo}
      mirror=${stateDir}/repo.git
      metrics=${stateDir}/metrics.prom
      ${lib.optionalString (cfg.sshKeyFile != null) ''
        export GIT_SSH_COMMAND="ssh -i ${cfg.sshKeyFile} -o BatchMode=yes -o IdentitiesOnly=yes${lib.optionalString (cfg.knownHosts != null) " -o UserKnownHostsFile=${pkgs.writeText "fleet-writer-known-hosts" cfg.knownHosts}"}"
      ''}

      write_metrics() { # <failure 0|1>
        local tmp commit=""
        if [ -e ${profileDir}/fleet-manifest ]; then
          commit=$(jq -r '.commit' "$(readlink -f ${profileDir}/fleet-manifest)")
        fi
        tmp=$(mktemp "$metrics.XXXXXX")
        {
          echo "# HELP fleet_writer_last_success_timestamp Unix time of the writer's last run that published or found nothing to publish."
          echo "# TYPE fleet_writer_last_success_timestamp gauge"
          echo "fleet_writer_last_success_timestamp $(cat ${stateDir}/last-success 2>/dev/null || echo 0)"
          echo "# HELP fleet_writer_last_failure 1 while the writer's last run failed (master not built or not published)."
          echo "# TYPE fleet_writer_last_failure gauge"
          echo "fleet_writer_last_failure $1"
          echo "# HELP fleet_manifest_commit_info Commit of the published manifest."
          echo "# TYPE fleet_manifest_commit_info gauge"
          [ -n "$commit" ] && echo "fleet_manifest_commit_info{commit=\"$commit\"} 1"
        } > "$tmp"
        mv -f "$tmp" "$metrics"
      }
      trap 'write_metrics 1' ERR

      head=$(git ls-remote "$repo" refs/heads/master | cut -f1)
      if [ -z "$head" ]; then
        echo "fleet-write: no master at $repo" >&2
        write_metrics 1
        exit 1
      fi
      if [ "$head" = "$(cat ${stateDir}/head 2>/dev/null || true)" ]; then
        date +%s > ${stateDir}/last-success
        write_metrics 0
        exit 0
      fi
      # A head that failed to build is retried after 10 minutes, not every
      # tick: a deterministic failure would otherwise rebuild every minute.
      # A transient one (nix-daemon restarted by nuc's own deploy) clears on
      # the retry.
      if [ "$head" = "$(cat ${stateDir}/failed-head 2>/dev/null || true)" ] &&
        [ -n "$(find ${stateDir}/failed-head -mmin -10)" ]; then
        echo "fleet-write: $head failed less than 10 minutes ago; not retrying yet" >&2
        write_metrics 1
        exit 1
      fi
      echo "$head" > ${stateDir}/failed-head

      [ -d "$mirror" ] || git init --quiet --bare "$mirror"
      git -C "$mirror" fetch --quiet "$repo" "+refs/heads/master:refs/heads/master"

      # Exactly the committed lock, as CI evaluates it; the git fetcher builds
      # exactly this commit from the object store.
      fleet=$(nix build --no-link --no-update-lock-file --print-out-paths \
        "git+file://$mirror?ref=master&rev=$head#${cfg.flakeAttr}")

      if [ "$fleet" = "$(readlink -f ${profileDir}/fleet 2>/dev/null || true)" ]; then
        echo "fleet-write: $head changes no system; nothing to publish"
      else
        prev=${pkgs.writeText "empty-manifest" "{}"}
        [ -e ${profileDir}/fleet-manifest ] && prev=$(readlink -f ${profileDir}/fleet-manifest)
        work=$(mktemp -d)
        trap 'rm -rf "$work"; write_metrics 1' ERR
        echo '{}' > "$work/hosts.json"
        for link in "$fleet"/*; do
          h=$(basename "$link")
          p=$(readlink -f "$link")
          sys=$(cat "$p/system")
          # A host's commit is the one that last changed its path.
          c=$(jq -r --arg h "$h" --arg p "$p" --arg head "$head" \
            'if .hosts[$h].path == $p then .hosts[$h].commit else $head end' "$prev")
          jq --arg h "$h" --arg p "$p" --arg s "$sys" --arg c "$c" \
            '.[$h] = {path: $p, system: $s, commit: $c}' "$work/hosts.json" > "$work/h.tmp"
          mv "$work/h.tmp" "$work/hosts.json"
        done
        jq -n --arg g "$(date -u +%Y-%m-%dT%H:%M:%SZ)" --arg c "$head" --slurpfile hosts "$work/hosts.json" \
          '{generated: $g, commit: $c, hosts: $hosts[0]}' > "$work/fleet-manifest.json"
        m=$(nix store add --mode flat "$work/fleet-manifest.json")
        rm -rf "$work"

        # Generation N roots the published set, N-1 the one before it (a host
        # mid-substitution, a one-step revert).
        nix-env -p ${profileDir}/fleet --set "$fleet"
        nix-env -p ${profileDir}/fleet --delete-generations +2
        nix-env -p ${profileDir}/fleet-manifest --set "$m"
        nix-env -p ${profileDir}/fleet-manifest --delete-generations +2

        printf '%s\n' "$m" > ${wwwDir}/.manifest.tmp
        mv -f ${wwwDir}/.manifest.tmp ${wwwDir}/manifest
        echo "fleet-write: published $m for $head"
      fi

      echo "$head" > ${stateDir}/head
      rm -f ${stateDir}/failed-head
      date +%s > ${stateDir}/last-success
      write_metrics 0
    '';
  };

  # Root copies the writer's metrics into node_exporter's directory, which
  # only root may write.
  publishMetrics = pkgs.writeShellScript "fleet-writer-metrics" ''
    set -eu
    src=${stateDir}/metrics.prom
    [ -e "$src" ] || exit 0
    dst=${config.nodeExporterTextfileDir}/fleet-writer.prom
    cp "$src" "$dst.tmp"
    chmod 0644 "$dst.tmp"
    mv -f "$dst.tmp" "$dst"
  '';
in {
  options.fleetCache = {
    enable = lib.mkEnableOption "harmonia and the manifest pointer (nginx /fleet/) on this host";
    signKeyFile = lib.mkOption {
      type = lib.types.str;
      description = "Private cache signing key (root-only): signs what this host builds and what harmonia serves.";
    };
  };

  options.fleetWriter = {
    enable = lib.mkEnableOption "the fleet writer (needs fleetCache)";
    repo = lib.mkOption {
      type = lib.types.str;
      description = "Git URL of the deploy source of truth (lab.services.nasMirror after the flip).";
    };
    sshKeyFile = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Read-only ssh key for `repo`, readable by fleet-writer.";
    };
    knownHosts = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "known_hosts lines for `repo`'s host.";
    };
    flakeAttr = lib.mkOption {
      type = lib.types.str;
      default = "fleet";
      description = "Flake attribute whose entries are the hosts' toplevels.";
    };
    interval = lib.mkOption {
      type = lib.types.str;
      default = "1min";
      description = "Time between polls of the deploy source.";
    };
  };

  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      assertions = [
        {
          assertion = config.fleetCache.enable;
          message = "fleetWriter needs fleetCache: it publishes into /var/www/fleet and its paths are served by harmonia.";
        }
      ];
      users.users.fleet-writer = {
        isSystemUser = true;
        group = "fleet-writer";
        home = stateDir;
      };
      users.groups.fleet-writer = {};

      systemd.tmpfiles.rules = [
        "d ${profileDir} 0755 fleet-writer fleet-writer -"
        "d ${wwwDir} 0755 fleet-writer fleet-writer -"
      ];

      systemd.services.fleet-writer = {
        description = "Publish the fleet manifest for the deploy source's master";
        path = [config.nix.package];
        serviceConfig = {
          Type = "oneshot";
          User = "fleet-writer";
          Group = "fleet-writer";
          StateDirectory = "fleet-writer";
          ExecStart = lib.getExe writer;
          ExecStopPost = "+${publishMetrics}";
        };
      };
      systemd.timers.fleet-writer = {
        wantedBy = ["timers.target"];
        timerConfig = {
          OnBootSec = "1min";
          OnUnitInactiveSec = cfg.interval;
        };
      };
    })

    (lib.mkIf config.fleetCache.enable {
      nix.settings.secret-key-files = [config.fleetCache.signKeyFile];

      services.harmonia = {
        cache = {
          enable = true;
          signKeyPaths = [config.fleetCache.signKeyFile];
          # Behind cache.nixos.org (priority 40) on every host: the CDN serves
          # the bulk, nuc what only it has.
          settings.priority = 50;
        };
      };

      # The LAN and the tailnet are trusted (nuc's own firewall is off; this
      # matters only where one is on).
      networking.firewall.allowedTCPPorts = [5000 5001];

      services.nginx = {
        enable = true;
        virtualHosts.fleet = {
          listen = [
            {
              addr = "0.0.0.0";
              port = 5001;
            }
            {
              addr = "[::]";
              port = 5001;
            }
          ];
          locations."/fleet/" = {
            alias = "${wwwDir}/";
            extraConfig = ''
              default_type text/plain;
              add_header Cache-Control "no-store";
            '';
          };
        };
      };
    })
  ];
}
