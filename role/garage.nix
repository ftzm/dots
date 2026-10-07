# Garage, the S3-compatible store on nas that CloudNativePG's Barman Cloud
# plugin archives the databases to: their base backups and WAL, off nuc and
# inside nas's borgbase job. One node (replication_factor 1): its redundancy
# is ZFS and borg, not Garage. Shared by nas and the Cluster Bootstrap
# rehearsal's fake nas (tests/cluster-bootstrap).
#
# Provisioning (layout, keys, buckets) is declared here and reconciled by
# garage-provision after every start: idempotent, create-only.
{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.garageNode;
  garage = pkgs.garage_2;
in {
  options.garageNode = {
    enable = lib.mkEnableOption "the Garage S3 node";
    dataDir = lib.mkOption {type = lib.types.str;};
    metadataDir = lib.mkOption {type = lib.types.str;};
    capacity = lib.mkOption {
      type = lib.types.str;
      default = "100G";
      description = "Capacity the single node's layout role declares.";
    };
    secretsFile = lib.mkOption {
      type = lib.types.path;
      description = "Environment file with GARAGE_RPC_SECRET and GARAGE_ADMIN_TOKEN.";
    };
    keys = lib.mkOption {
      type = lib.types.attrsOf lib.types.path;
      default = {};
      description = ''
        Access keys by name: a JSON file {"accessKeyId": "GK...", "secretAccessKey": "..."}
        each, imported through the admin API (never on a command line).
      '';
    };
    buckets = lib.mkOption {
      type = lib.types.attrsOf (lib.types.listOf lib.types.str);
      default = {};
      description = "Buckets, each with the key names given read and write on it.";
    };
  };

  config = lib.mkIf cfg.enable {
    services.garage = {
      enable = true;
      package = garage;
      environmentFile = cfg.secretsFile;
      settings = {
        metadata_dir = cfg.metadataDir;
        data_dir = cfg.dataDir;
        # Robust to unclean shutdown, where LMDB "has a tendency of becoming
        # corrupted" (Garage's real-world guide); this load needs no speed.
        db_engine = "sqlite";
        # Consistent copies of the metadata DB inside metadata_dir: a file
        # backup (nas's borg job) of the live database could catch it
        # mid-write; a snapshot it can restore from is always complete.
        metadata_auto_snapshot_interval = "6h";
        replication_factor = 1;
        rpc_bind_addr = "[::]:3901";
        rpc_public_addr = "127.0.0.1:3901";
        s3_api = {
          api_bind_addr = "[::]:3900";
          s3_region = "garage";
        };
        admin.api_bind_addr = "127.0.0.1:3903";
      };
    };
    # The module's DynamicUser gets a new uid per start, which cannot own data
    # on the pool; a fixed system user can.
    users.users.garage = {
      isSystemUser = true;
      group = "garage";
    };
    users.groups.garage = {};
    systemd.services.garage.serviceConfig = {
      DynamicUser = lib.mkForce false;
      User = "garage";
      Group = "garage";
    };
    systemd.tmpfiles.rules = [
      "d ${cfg.dataDir} 0750 garage garage -"
      "d ${cfg.metadataDir} 0750 garage garage -"
    ];
    networking.firewall.allowedTCPPorts = [3900];

    systemd.services.garage-provision = {
      description = "Garage layout, keys and buckets";
      after = ["garage.service"];
      requires = ["garage.service"];
      wantedBy = ["multi-user.target"];
      path = [garage pkgs.curl pkgs.jq pkgs.gnugrep pkgs.gnused];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        EnvironmentFile = cfg.secretsFile;
      };
      script = ''
        set -euo pipefail
        until garage status >/dev/null 2>&1; do sleep 1; done
        if garage status | grep -q 'NO ROLE ASSIGNED'; then
          id=$(garage node id -q | cut -d@ -f1)
          garage layout assign -z nas -c ${cfg.capacity} "$id"
          v=$(garage layout show | sed -n 's/.*[Cc]urrent cluster layout version: *\([0-9]*\).*/\1/p')
          garage layout apply --version $((''${v:-0} + 1))
        fi
        hdr=$(mktemp)
        trap 'rm -f "$hdr"' EXIT
        printf 'Authorization: Bearer %s\n' "$GARAGE_ADMIN_TOKEN" >"$hdr"
        api=http://127.0.0.1:3903
        ${lib.concatStrings (lib.mapAttrsToList (name: file: ''
            if ! curl -sf -H @"$hdr" "$api/v2/GetKeyInfo?search=${name}" >/dev/null; then
              jq --arg n ${name} '. + {name: $n}' ${file} |
                curl -sf -H @"$hdr" -H 'Content-Type: application/json' --data @- "$api/v2/ImportKey" >/dev/null
              echo "imported key ${name}"
            fi
          '')
          cfg.keys)}
        ${lib.concatStrings (lib.mapAttrsToList (bucket: keys: ''
            garage bucket info ${bucket} >/dev/null 2>&1 || garage bucket create ${bucket}
            ${lib.concatMapStrings (k: ''
                garage bucket allow --read --write --key ${k} ${bucket} >/dev/null
              '')
              keys}
          '')
          cfg.buckets)}
      '';
    };
  };
}
