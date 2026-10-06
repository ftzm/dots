# A host deployed by fleet-agent from nuc's manifest (FORGEJO_MIGRATION_PLAN.md
# -> Binary Cache), wired to the lab inventory: the LAN endpoints for the lab
# hosts, the Tailscale ones for the laptops, nuc's cache key as the one trust
# root.
{
  config,
  lab,
  lib,
  pkgs,
  ...
}: let
  cfg = config.fleetHost;

  # The cut-over's commit B removes comin while comin itself applies it; its
  # unit carried X-StopOnRemoval=false, so the switch left it running. Stop it
  # once no switch is in flight, drop its profile (its generations root old
  # systems and list boot entries) once the system profile names what runs,
  # and rewrite the boot entries without it. A no-op on a host without comin.
  retireComin = pkgs.writeShellScript "fleet-retire-comin" ''
    set -eu
    PATH=${lib.makeBinPath [config.systemd.package pkgs.procps pkgs.coreutils]}
    if systemctl is-active -q comin.service; then
      if pgrep -f switch-to-configuration >/dev/null; then
        echo "fleet-retire-comin: a switch is in flight; next tick"
        exit 0
      fi
      systemctl stop comin.service
      echo "fleet-retire-comin: stopped comin"
    fi
    if ls /nix/var/nix/profiles/system-profiles/comin* >/dev/null 2>&1 &&
      [ "$(readlink -f /nix/var/nix/profiles/system)" = "$(readlink -f /run/current-system)" ]; then
      rm -f /nix/var/nix/profiles/system-profiles/comin*
      /run/current-system/bin/switch-to-configuration boot
      echo "fleet-retire-comin: removed comin's profile, boot entries rewritten"
    fi
  '';
in {
  imports = [./fleet-agent.nix ./lab.nix ./node-exporter.nix];

  options.fleetHost = {
    transport = lib.mkOption {
      type = lib.types.enum ["lan" "tailscale"];
      description = "How this host reaches nuc: the LAN (always-on lab hosts) or Tailscale (laptops, at home and away).";
    };
    autoRebootAt = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      example = "04:30";
      description = "Always-on hosts only: daily time to reboot into a deferred switch (fleetAgent.autoReboot). Stagger hosts that depend on each other.";
    };
    keepGenerations = lib.mkOption {
      type = lib.types.ints.positive;
      default = 5;
      description = "System generations kept after each deploy.";
    };
  };

  config.systemd.services.fleet-retire-comin = {
    description = "Retire the comin the cut-over left running";
    serviceConfig = {
      Type = "oneshot";
      ExecStart = retireComin;
    };
  };
  config.systemd.timers.fleet-retire-comin = {
    wantedBy = ["timers.target"];
    timerConfig = {
      OnActiveSec = "1min";
      OnUnitActiveSec = "1min";
    };
  };

  config.fleetAgent = {
    enable = true;
    manifestUrl =
      if cfg.transport == "lan"
      then lab.services.fleetManifestLan
      else lab.services.fleetManifestTailscale;
    cacheUrl =
      if cfg.transport == "lan"
      then lab.services.fleetCacheLan
      else lab.services.fleetCacheTailscale;
    # nuc's cache signing key (secrets/fleet-cache-key.age).
    cachePublicKey = "nuc-fleet-1:UsrDofmvTsoT/qyGk/U1C+nabt65gxr9e7gAxJy0icM=";
    inherit (cfg) keepGenerations;
    autoReboot = lib.mkIf (cfg.autoRebootAt != null) {
      enable = true;
      at = cfg.autoRebootAt;
    };
  };
}
