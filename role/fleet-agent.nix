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
# (the activation script links it before the unit phase). Such a path is
# retried once, 30 minutes after the failure: a transient clears, a broken
# unit or a hang costs one more bounded attempt. After that the failure stays
# reported until a later activation succeeds or is deferred, or the host
# boots the flagged path (which completes the switch).
#
# Metrics, in node_exporter's textfile dir: fleet_deployed_commit_info,
# fleet_last_success_timestamp, fleet_last_failure.
#
# autoReboot (always-on hosts only): a nightly timer reboots the host when a
# deferred switch is pending -- the system profile differs from what runs and
# no activation failed -- once per installed path, so a system that does not
# come up as itself is never rebooted into again by this. Laptops leave it
# off and keep the DeferredSwitchNeedsReboot alert.
{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.fleetAgent;
  stateDir = "/var/lib/fleet-agent";

  rebooter = pkgs.writeShellApplication {
    name = "fleet-agent-reboot";
    runtimeInputs = [pkgs.coreutils config.systemd.package];
    text = ''
      installed=$(readlink -f /nix/var/nix/profiles/system)
      current=$(readlink -f /run/current-system)
      if [ "$installed" = "$current" ]; then
        exit 0
      fi
      if [ -e ${stateDir}/failed ]; then
        echo "fleet-agent-reboot: an activation failed; not rebooting into $installed"
        exit 0
      fi
      if [ "$(cat ${stateDir}/rebooted-for 2>/dev/null || true)" = "$installed" ]; then
        echo "fleet-agent-reboot: already rebooted once for $installed and it is not running; leaving it to the operator" >&2
        exit 1
      fi
      echo "$installed" > ${stateDir}/rebooted-for
      echo "fleet-agent-reboot: deferred switch pending, rebooting into $installed"
      systemctl reboot
    '';
  };

  agent = pkgs.writeShellApplication {
    name = "fleet-agent";
    runtimeInputs = [config.nix.package pkgs.curl pkgs.jq pkgs.coreutils pkgs.findutils config.systemd.package];
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
        if [ "$flagged" != "$path" ]; then
          # The system profile is what boots next and what the deferred-reboot
          # check and autoReboot compare against. A system another deployer
          # activated (comin, until the cut-over; a manual rebuild) leaves it
          # behind: point it at what runs, or a reboot would boot that older
          # generation.
          if ! ${lib.boolToString cfg.dryRun} && [ "$installed" != "$current" ]; then
            echo "fleet-agent: system profile was $installed; pointing it at the running $current"
            nix-env -p /nix/var/nix/profiles/system --set "$current"
            prune
          fi
          converged "$commit"
          exit 0
        fi
        # Current but its activation failed. Retried once, 30 minutes on: a
        # transient (a user session closing mid-switch) clears; a broken unit
        # or a hang costs one more bounded attempt and then stays reported.
        if [ "$(cat ${stateDir}/retried 2>/dev/null || true)" = "$path" ] ||
          [ -z "$(find "$flag" -mmin +30)" ]; then
          echo "fleet-agent: $path is current but its activation failed; not retrying now (reboot or a new commit clears it)"
          write_metrics
          exit 0
        fi
        echo "$path" > ${stateDir}/retried
        echo "fleet-agent: retrying the failed activation of $path once"
        if activate "$path" switch; then
          rm -f "$flag"
          converged "$commit"
          exit 0
        fi
        failed "$path"
        exit 1
      fi
      generated=$(date -d "$(jq -er '.generated' "$ptr")" +%s)
      activated=$(stat -c %Y /run/current-system)

      # A running system activated after this manifest was generated came from
      # another deployer -- comin applying a commit before the writer has
      # published it (the cut-over), or a manual rebuild. Reverting it at once
      # would flip the host back and forth; leave it for foreignGraceSeconds
      # (30 minutes), in which the writer normally publishes what runs. After
      # that the manifest wins.
      if [ "$activated" -gt "$generated" ] && [ $(($(date +%s) - activated)) -lt ${toString cfg.foreignGraceSeconds} ]; then
        echo "fleet-agent: the running system was activated after this manifest (by another deployer); not replacing it before the next manifest or ${toString cfg.foreignGraceSeconds}s"
        write_metrics
        exit 0
      fi

      # A deferred switch: the agent installed the path as the boot default and
      # the running system predates the manifest. (A system activated after
      # it is a manual switch, handled above, even if the profile still names
      # the manifest's path.)
      if [ "$path" = "$installed" ] && [ -z "$flagged" ] && [ "$activated" -le "$generated" ]; then
        echo "fleet-agent: $path installed, waiting for a reboot (deferred switch)"
        converged "$commit"
        exit 0
      fi


      if ${lib.boolToString cfg.dryRun}; then
        # Observing only (another deployer still owns this host): fetch and
        # verify, so the closure is here when the agent takes over, but
        # activate nothing and claim no deploy.
        echo "fleet-agent: dry run -- would deploy $path (commit $commit)"
        nix-store --realise "$path" --add-root ${stateDir}/system >/dev/null
        date +%s > ${stateDir}/last-success
        write_metrics
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
    foreignGraceSeconds = lib.mkOption {
      type = lib.types.ints.positive;
      default = 1800;
      description = "How long a running system activated after the manifest (by comin, or by hand) is left alone before the manifest replaces it.";
    };
    dryRun = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Fetch, verify and download what the manifest names, but activate nothing -- while another deployer (comin) still owns the host.";
    };
    autoReboot = {
      enable = lib.mkEnableOption "rebooting into a deferred switch in a nightly window (always-on hosts only)";
      at = lib.mkOption {
        type = lib.types.str;
        example = "04:30";
        description = "Daily time (systemd OnCalendar) to reboot when a deferred switch is pending. Stagger hosts that depend on each other.";
      };
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
    systemd.services.fleet-agent-reboot = lib.mkIf (cfg.autoReboot.enable && !cfg.dryRun) {
      description = "Reboot into a deferred fleet deploy";
      serviceConfig = {
        Type = "oneshot";
        ExecStart = lib.getExe rebooter;
        StateDirectory = "fleet-agent";
      };
    };
    systemd.timers.fleet-agent-reboot = lib.mkIf (cfg.autoReboot.enable && !cfg.dryRun) {
      wantedBy = ["timers.target"];
      # No Persistent: a host that was off at the window must not reboot as
      # soon as it boots.
      timerConfig.OnCalendar = cfg.autoReboot.at;
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
