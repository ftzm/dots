# Weekly self-tests of the two manual deploy paths that stand in for the
# automation when nuc -- the deploy plane -- is down (FORGEJO_MIGRATION_PLAN.md
# -> nuc Down): a laptop rebuilding itself from its own clone, and the
# workstation pushing a closure to a lab host. Each runs in a form that
# changes nothing, so a stale clone, a missing key or a broken sudo surfaces
# on a quiet Monday instead of during an outage.
#
#   laptop-rebuild  `git fetch` in the clone, then `nixos-rebuild build` of
#                   origin/master for this host: fetch, evaluation, substitution.
#   workstation-push  `nixos-rebuild dry-activate --target-host` of origin/master
#                   for each lab host: ssh, sudo, the build and the copy,
#                   nothing activated.
#
# Each (path, host) pair reports fleet_manual_path_last_run_success and
# fleet_manual_path_last_success_timestamp through node_exporter's textfile
# collector; the Fleet* rules in cluster lab.jsonnet alert on a failed run and
# on a success older than 8 days.
#
# A failed run retries hourly, so a passing network blip clears itself before
# the failure alert's `for` elapses. A push target that does not answer as
# itself (its ssh host key) is not a run at all: the workstation is away from
# the home LAN, or the host is down, which its own alerts cover. Nothing is
# recorded for it, the unit retries hourly, and a target never reached for
# 8 days trips the staleness alert.
{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.manualPathSelftest;
  host = config.networking.hostName;
  stateDir = "/var/lib/manual-path-selftest";

  # Exit status for "nothing could be tested": clean for systemd (no failed
  # unit) but restarted like a failure. EX_TEMPFAIL.
  tempfail = 75;

  common = ''
    set -uo pipefail
    clone=${lib.escapeShellArg cfg.clone}
    record() { # <path> <host> ok|fail
      if [ "$3" = ok ]; then
        date +%s > "${stateDir}/$1.$2.last-success"
        echo 1 > "${stateDir}/$1.$2.last-run"
      else
        echo 0 > "${stateDir}/$1.$2.last-run"
      fi
    }
    # Both units fetch the same clone; git refuses a second concurrent fetch.
    fetch() {
      flock "${stateDir}/fetch.lock" git -C "$clone" fetch --quiet origin &&
        git -C "$clone" rev-parse --verify origin/master
    }
    flake() { # <rev> <host>
      echo "git+file://$clone?rev=$1#$2"
    }
  '';

  laptopScript = pkgs.writeShellScript "manual-path-selftest-laptop" ''
    ${common}
    if rev=$(fetch) &&
      nixos-rebuild build --no-reexec --accept-flake-config \
        --flake "$(flake "$rev" ${host})"; then
      record laptop-rebuild ${host} ok
    else
      record laptop-rebuild ${host} fail
      exit 1
    fi
  '';

  pushScript = pkgs.writeShellScript "manual-path-selftest-push" ''
    ${common}
    key=${lib.escapeShellArg cfg.push.sshKey}
    export NIX_SSHOPTS="-i $key -o BatchMode=yes -o ConnectTimeout=10"
    failed=0
    skipped=0
    rev=
    ${lib.concatStrings (lib.mapAttrsToList (target: dest: ''
        addr=${lib.escapeShellArg (lib.last (lib.splitString "@" dest))}
        known=$(ssh-keygen -F "$addr" | awk '$2 == "ssh-ed25519" {print $3}')
        seen=$(ssh-keyscan -T 10 -t ed25519 "$addr" 2>/dev/null | awk '$2 == "ssh-ed25519" {print $3}')
        if [ -z "$known" ] || [ "$seen" != "$known" ]; then
          echo "${target}: $addr does not answer with its known host key; away from the home LAN or ${target} is down -- not a run"
          skipped=1
        elif [ -z "$rev" ] && ! rev=$(fetch); then
          record workstation-push ${target} fail
          failed=1
        elif nixos-rebuild dry-activate --no-reexec --accept-flake-config \
          --flake "$(flake "$rev" ${target})" \
          --target-host ${lib.escapeShellArg dest} --sudo; then
          record workstation-push ${target} ok
        else
          record workstation-push ${target} fail
          failed=1
        fi
      '')
      cfg.push.targets)}
    [ "$failed" = 0 ] || exit 1
    [ "$skipped" = 0 ] || exit ${toString tempfail}
  '';

  # Root renders every recorded pair into one file; node_exporter reads the
  # directory continuously, so write then rename.
  renderScript = pkgs.writeShellScript "manual-path-selftest-render" ''
    set -eu
    out=${config.nodeExporterTextfileDir}/fleet-manual-path.prom
    tmp=$(mktemp "$out.XXXXXX")
    {
      echo "# HELP fleet_manual_path_last_run_success Last self-test run of a manual deploy path (1 = passed)."
      echo "# TYPE fleet_manual_path_last_run_success gauge"
      for f in ${stateDir}/*.last-run; do
        [ -e "$f" ] || continue
        id=$(basename "$f" .last-run)
        echo "fleet_manual_path_last_run_success{path=\"''${id%%.*}\",host=\"''${id#*.}\"} $(cat "$f")"
      done
      echo "# HELP fleet_manual_path_last_success_timestamp Unix time of the last passing self-test of a manual deploy path (0 = never)."
      echo "# TYPE fleet_manual_path_last_success_timestamp gauge"
      for f in ${stateDir}/*.last-run; do
        [ -e "$f" ] || continue
        id=$(basename "$f" .last-run)
        ts=0
        [ -e "${stateDir}/$id.last-success" ] && ts=$(cat "${stateDir}/$id.last-success")
        echo "fleet_manual_path_last_success_timestamp{path=\"''${id%%.*}\",host=\"''${id#*.}\"} $ts"
      done
    } > "$tmp"
    chmod 0644 "$tmp"
    mv -f "$tmp" "$out"
  '';

  unit = name: script: {
    description = "Self-test of a manual deploy path (changes nothing)";
    path = [
      config.system.build.nixos-rebuild
      config.nix.package
      pkgs.git
      pkgs.openssh
      pkgs.util-linux
      pkgs.gawk
      pkgs.coreutils
    ];
    environment.HOME = config.users.users.${cfg.user}.home;
    serviceConfig = {
      Type = "oneshot";
      User = cfg.user;
      StateDirectory = "manual-path-selftest";
      # `result` links land here and go with the unit.
      RuntimeDirectory = name;
      WorkingDirectory = "/run/${name}";
      ExecStart = script;
      ExecStopPost = "+${renderScript}";
      Restart = "on-failure";
      RestartSec = "1h";
      SuccessExitStatus = tempfail;
      RestartForceExitStatus = tempfail;
      Nice = 19;
      IOSchedulingClass = "idle";
    };
    # A retried oneshot must never hit the start limit.
    startLimitIntervalSec = 0;
  };

  timer = {
    wantedBy = ["timers.target"];
    timerConfig = {
      OnCalendar = "weekly";
      # Laptops are off a lot: a missed run happens right after boot, which
      # the Fleet* rules' `for: 2h` allows for.
      Persistent = true;
      RandomizedDelaySec = "10min";
    };
  };
in {
  options.manualPathSelftest = {
    laptop.enable = lib.mkEnableOption "the weekly laptop self-rebuild self-test";
    push.targets = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = {};
      example = {nas = "admin@192.168.1.3";};
      description = "Lab hosts for the weekly workstation-push self-test: flake host name -> ssh destination (by IP, whose ed25519 host key is in the user's known_hosts).";
    };
    push.sshKey = lib.mkOption {
      type = lib.types.str;
      default = "${config.users.users.${cfg.user}.home}/.ssh/id_rsa";
      description = "Passphrase-less key the push self-test logs in with (no agent in a unit).";
    };
    clone = lib.mkOption {
      type = lib.types.str;
      default = "${config.users.users.${cfg.user}.home}/dots";
      description = "The user's clone of dots, fetched and built from.";
    };
    user = lib.mkOption {
      type = lib.types.str;
      default = "ftzm";
      description = "Owner of the clone and the ssh key.";
    };
  };

  config = lib.mkMerge [
    (lib.mkIf cfg.laptop.enable {
      systemd.services.manual-path-selftest-laptop = unit "manual-path-selftest-laptop" laptopScript;
      systemd.timers.manual-path-selftest-laptop = timer;
    })
    (lib.mkIf (cfg.push.targets != {}) {
      systemd.services.manual-path-selftest-push = unit "manual-path-selftest-push" pushScript;
      systemd.timers.manual-path-selftest-push = timer;
    })
  ];
}
