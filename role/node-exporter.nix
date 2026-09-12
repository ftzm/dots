# Prometheus node exporter for the whole fleet, plus the one fact nothing else
# publishes: whether the booted system still matches the deployed one, and why
# it doesn't.
#
# comin_need_to_reboot already says *that* a reboot is pending. It cannot say
# what for, and an alert that cannot name its cause gets filed as noise --
# saoiste ran a stale kernel for four days (2026-09-08 to 2026-09-12) behind a
# notification whose entire body was "A deployment is pending a reboot on
# saoiste". The kernel versions ARE the message, so they have to arrive as
# labels; node_exporter's textfile collector is the cheapest way to get
# machine-local facts into Prometheus without writing an exporter.
{
  config,
  lib,
  pkgs,
  ...
}: let
  textfileDir = "/var/lib/prometheus-node-exporter-text-files";

  rebootMetrics = pkgs.writeShellScript "nixos-reboot-required-metrics" ''
    set -eu

    # A kernel lives at /nix/store/<hash>-linux-<ver>/bzImage and its initrd at
    # /nix/store/<hash>-initrd-linux-<ver>/initrd, so the same strip yields the
    # version from either.
    version() {
      name=$(basename "$(dirname "$(readlink -f "$1")")")
      printf '%s' "''${name#*-linux-}"
    }

    booted_kernel=$(version /run/booted-system/kernel)
    current_kernel=$(version /run/current-system/kernel)

    # Compare store paths, not versions: an initrd can be rebuilt without the
    # kernel version moving, and that still needs a reboot to take effect.
    bk=$(readlink -f /run/booted-system/kernel)
    ck=$(readlink -f /run/current-system/kernel)
    bi=$(readlink -f /run/booted-system/initrd)
    ci=$(readlink -f /run/current-system/initrd)

    reason=""
    if [ "$bk" != "$ck" ]; then
      reason="kernel"
    elif [ "$bi" != "$ci" ]; then
      reason="initrd"
    fi

    if [ -n "$reason" ]; then value=1; else value=0; fi

    # node_exporter reads this directory continuously, so a half-written file
    # would be scraped as truncated garbage. Write then rename.
    tmp=$(mktemp "${textfileDir}/.nixos-reboot-required.XXXXXX")
    {
      echo "# HELP nixos_reboot_required Booted system differs from the deployed one in a way only a reboot applies (1 = reboot needed)."
      echo "# TYPE nixos_reboot_required gauge"
      echo "nixos_reboot_required{booted_kernel=\"$booted_kernel\",current_kernel=\"$current_kernel\",reason=\"$reason\"} $value"
    } > "$tmp"
    chmod 0644 "$tmp"
    mv -f "$tmp" "${textfileDir}/nixos-reboot-required.prom"
  '';
in {
  services.prometheus.exporters.node = {
    enable = true;
    enabledCollectors = ["processes" "systemd" "textfile"];
    extraFlags = [
      # Without this the systemd collector exports no restart counters, so the
      # shipped NodeSystemdServiceCrashlooping rule can never fire -- nas alloy
      # restarting every 2s for 20h (2026-09-04) produced no metric at all.
      "--collector.systemd.enable-restarts-metrics"
      "--collector.textfile.directory=${textfileDir}"
    ];
    port = 9002;
  };

  systemd.tmpfiles.rules = ["d ${textfileDir} 0755 root root -"];

  systemd.services.nixos-reboot-required-metrics = {
    description = "Publish booted-vs-deployed kernel facts for node_exporter";
    serviceConfig = {
      Type = "oneshot";
      ExecStart = rebootMetrics;
    };
  };

  # A timer rather than an activation hook on purpose: the unit's own
  # definition does not change between deploys, so a switch would not restart
  # it, and the file would keep describing the previous generation. Polling
  # costs nothing and the alert it feeds waits an hour anyway.
  systemd.timers.nixos-reboot-required-metrics = {
    wantedBy = ["timers.target"];
    timerConfig = {
      OnBootSec = "1min";
      OnUnitActiveSec = "5min";
      Unit = "nixos-reboot-required-metrics.service";
    };
  };

  # Scraped over tailscale on the roaming machines and over the LAN on the lab
  # ones. Interface-scoped so it is a no-op where the firewall is disabled,
  # rather than a hole that only shows up if one is ever turned on.
  networking.firewall.interfaces."tailscale0".allowedTCPPorts = [9002];
}
