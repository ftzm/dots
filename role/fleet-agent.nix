# fleet-agent: deploys this host from the manifest nuc publishes
# (FORGEJO_MIGRATION_PLAN.md -> Binary Cache -> Design -> Agent).
#
# Every 2 minutes: fetch the pointer (one line: the manifest's store path),
# realise the manifest from the cache -- nix checks its signature against the
# cache key, the one trust root -- read this host's entry, and, if its path is
# not what runs, substitute it and activate it. No git, no evaluation, no
# build on the host.
#
# Activation is `boot`, then `switch`, each in its own transient unit
# (systemd-run, as nixos-rebuild does), so a commit that changes this unit
# cannot kill the switch it triggers, and each bounded by RuntimeMaxSec so a
# hung activation is reported instead of blocking the timer forever. `boot`
# first leaves the new generation the boot default even if `switch` dies.
#
# A deferred switch -- a switch inhibitor changed, or `switch` exits 100 (the
# new systemd cannot replace the running PID 1 live) -- is success: the boot
# entry is installed, the host runs it from its next reboot, and
# nixos_reboot_required{reason="deferred"} reports the pending reboot
# (role/node-exporter.nix).
#
# A failed activation is remembered in /var/lib/fleet-agent/failed (the
# store path). It can leave /run/current-system pointing at the failed path
# (the activation script links it before the unit phase), so the agent does
# not retry a path that is already current; the failure stays reported until
# a later activation succeeds or is deferred, or the host boots the flagged
# path (which completes the switch).
#
# Metrics, in node_exporter's textfile dir: fleet_deployed_commit_info,
# fleet_last_success_timestamp, fleet_last_failure.
{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.fleetAgent;
  stateDir = "/var/lib/fleet-agent";

  agent = pkgs.writeShellApplication {
    name = "fleet-agent";
    runtimeInputs = [config.nix.package pkgs.curl pkgs.jq pkgs.coreutils config.systemd.package];
    text = ''
      host=${lib.escapeShellArg config.networking.hostName}
      url=${lib.escapeShellArg cfg.manifestUrl}
      flag=${stateDir}/failed
      deployed_file=${stateDir}/deployed-commit
      metrics=${config.nodeExporterTextfileDir}/fleet-agent.prom
      failure=0

      write_metrics() {
        local tmp commit
        commit=$(cat "$deployed_file" 2>/dev/null || true)
        tmp=$(mktemp "$metrics.XXXXXX")
        {
          echo "# HELP fleet_deployed_commit_info Manifest commit this host runs or has installed as its boot default."
          echo "# TYPE fleet_deployed_commit_info gauge"
          [ -n "$commit" ] && echo "fleet_deployed_commit_info{commit=\"$commit\"} 1"
          echo "# HELP fleet_last_success_timestamp Unix time of the agent's last run that ended converged (or deferred)."
          echo "# TYPE fleet_last_success_timestamp gauge"
          echo "fleet_last_success_timestamp $(cat ${stateDir}/last-success 2>/dev/null || echo 0)"
          echo "# HELP fleet_last_failure 1 while the last activation of the manifest's path for this host failed."
          echo "# TYPE fleet_last_failure gauge"
          echo "fleet_last_failure $failure"
        } > "$tmp"
        chmod 0644 "$tmp"
        mv -f "$tmp" "$metrics"
      }

      converged() { # <manifest commit>
        echo "$1" > "$deployed_file"
        date +%s > ${stateDir}/last-success
        failure=0
        write_metrics
      }

      failed() { # <store path>
        echo "$1" > "$flag"
        failure=1
        write_metrics
      }

      activate() { # <path> <action>
        systemd-run --wait --pipe --collect --quiet --service-type=exec \
          --unit=fleet-agent-switch -p RuntimeMaxSec=${cfg.activationTimeout} \
          "$1/bin/switch-to-configuration" "$2"
      }

      # Keys present in both systems' switch-inhibitors whose value changed:
      # switch-to-configuration would refuse the switch (exit 1) on them.
      inhibited() { # <new path>
        local cur=/run/current-system/switch-inhibitors new="$1/switch-inhibitors"
        jq -en \
          --argjson a "$( [ -e "$cur" ] && cat "$cur" || echo '{}')" \
          --argjson b "$( [ -e "$new" ] && cat "$new" || echo '{}')" \
          '[$a | keys[] as $k | select($b | has($k)) | select($a[$k] != $b[$k])] | length > 0' >/dev/null
      }

      prune() {
        nix-env -p /nix/var/nix/profiles/system --delete-generations +${toString cfg.keepGenerations}
      }

      [ -f "$flag" ] && failure=1

      ptr=$(curl -fsS --max-time 20 "$url")
      if ! [[ $ptr =~ ^/nix/store/[a-z0-9]{32}-[^/[:space:]]+$ ]]; then
        echo "fleet-agent: $url returned no store path" >&2
        exit 1
      fi
      # Substituted from the cache with its signature checked, or already here.
      # Rooted, so an unchanged tick finds it locally and asks nothing more.
      nix-store --realise "$ptr" --add-root ${stateDir}/manifest >/dev/null
      commit=$(jq -er '.commit' "$ptr")
      path=$(jq -er --arg h "$host" '.hosts[$h].path' "$ptr")

      current=$(readlink -f /run/current-system)
      booted=$(readlink -f /run/booted-system)
      installed=$(readlink -f /nix/var/nix/profiles/system)
      flagged=$(cat "$flag" 2>/dev/null || true)

      # Booting the flagged system completes its switch.
      if [ -n "$flagged" ] && [ "$booted" = "$flagged" ]; then
        rm -f "$flag"
        flagged=""
        failure=0
      fi

      if [ "$path" = "$current" ]; then
        if [ "$flagged" = "$path" ]; then
          echo "fleet-agent: $path is current but its activation failed; not retrying (reboot or a new commit clears it)"
          write_metrics
        else
          converged "$commit"
        fi
        exit 0
      fi
      if [ "$path" = "$installed" ] && [ -z "$flagged" ]; then
        echo "fleet-agent: $path installed, waiting for a reboot (deferred switch)"
        converged "$commit"
        exit 0
      fi

      echo "fleet-agent: deploying $path (commit $commit)"
      # Rooted from the moment it is valid: the auto-GC a min-free host runs
      # inside this very call must not collect it before the profile does.
      nix-store --realise "$path" --add-root ${stateDir}/system >/dev/null
      nix-env -p /nix/var/nix/profiles/system --set "$path"

      if ! activate "$path" boot; then
        failed "$path"
        exit 1
      fi
      if inhibited "$path"; then
        echo "fleet-agent: switch inhibitors differ; $path is the boot default, deferred to the next reboot"
        rm -f "$flag"
        prune
        converged "$commit"
        exit 0
      fi
      rc=0
      activate "$path" switch || rc=$?
      case $rc in
        0)
          rm -f "$flag"
          prune
          converged "$commit"
          ;;
        100)
          echo "fleet-agent: the new systemd cannot be switched to live; $path is the boot default, deferred to the next reboot"
          rm -f "$flag"
          prune
          converged "$commit"
          ;;
        *)
          failed "$path"
          exit 1
          ;;
      esac
    '';
  };
in {
  options.fleetAgent = {
    enable = lib.mkEnableOption "deploying this host from nuc's fleet manifest";
    manifestUrl = lib.mkOption {
      type = lib.types.str;
      description = "URL of the manifest pointer (lab.services.fleetManifestLan or fleetManifestTailscale).";
    };
    cacheUrl = lib.mkOption {
      type = lib.types.str;
      description = "nuc's binary cache (lab.services.fleetCacheLan or fleetCacheTailscale).";
    };
    cachePublicKey = lib.mkOption {
      type = lib.types.str;
      description = "Public half of nuc's cache signing key; it verifies every path from the cache, the manifest included.";
    };
    keepGenerations = lib.mkOption {
      type = lib.types.ints.positive;
      default = 5;
      description = "System generations kept after each deploy (>= 2 keeps a rollback target in the boot menu).";
    };
    interval = lib.mkOption {
      type = lib.types.str;
      default = "2min";
      description = "Time between agent runs.";
    };
    activationTimeout = lib.mkOption {
      type = lib.types.str;
      default = "15min";
      description = "RuntimeMaxSec of each activation (boot, switch).";
    };
  };

  config = lib.mkIf cfg.enable {
    nix.settings = {
      # cache.nixos.org first for what it has -- the bulk, over its CDN even
      # off-LAN -- then nuc for the rest. Order is by priority: harmonia
      # serves priority 50 (role/fleet-writer.nix), cache.nixos.org 40.
      substituters = lib.mkAfter [cfg.cacheUrl];
      trusted-public-keys = [cfg.cachePublicKey];
      # A dark home must not stall every nix command.
      connect-timeout = 5;
      # Free space before the store fills.
      min-free = lib.mkDefault (1024 * 1024 * 1024);
      max-free = lib.mkDefault (5 * 1024 * 1024 * 1024);
    };
    # Every deploy is a new system generation; the agent prunes to
    # keepGenerations and this frees the paths. No age option:
    # nix-collect-garbage cannot keep a count, the agent does.
    nix.gc = {
      automatic = true;
      dates = "weekly";
      options = "";
    };

    systemd.services.fleet-agent = {
      description = "Deploy this host from the fleet manifest";
      serviceConfig = {
        Type = "oneshot";
        ExecStart = lib.getExe agent;
        StateDirectory = "fleet-agent";
      };
      # Not restarted by the switch it performs.
      restartIfChanged = false;
    };
    systemd.timers.fleet-agent = {
      wantedBy = ["timers.target"];
      timerConfig = {
        OnBootSec = "1min";
        OnUnitInactiveSec = cfg.interval;
      };
    };
  };
}
