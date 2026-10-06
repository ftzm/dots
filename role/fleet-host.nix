# A host deployed by fleet-agent from nuc's manifest (FORGEJO_MIGRATION_PLAN.md
# -> Binary Cache), wired to the lab inventory: the LAN endpoints for the lab
# hosts, the Tailscale ones for the laptops, nuc's cache key as the one trust
# root.
{
  config,
  lab,
  lib,
  ...
}: let
  cfg = config.fleetHost;
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
