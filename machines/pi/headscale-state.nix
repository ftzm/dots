# headscale's runtime state, which no config can declare: the nodes'
# registrations and the tailnet IPs headscale allocated them (nuc's
# 100.64.0.2 is hardcoded in role/lab.nix, the laptops' in the cluster's scrape
# config, and the 0.28 CLI cannot pin an address). A rebuilt server that
# started empty would hand out addresses by registration order.
#
# - Daily: a consistent SQLite snapshot (`.backup`, safe under a running
#   headscale and WAL) to the nas over NFS, which the nas's borgbase job
#   carries off-site.
# - Before every headscale start: if the database is missing and the nas has
#   a snapshot, restore it -- a replaced SD card comes back with the same
#   nodes and IPs. If the nas is unreachable, refuse to start rather than
#   start empty. If the version changed, snapshot locally before headscale
#   migrates the schema (an older headscale cannot read a migrated one).
#
# The server's identity, its noise private key, is declared
# (headscaleState.noiseKeyFile; the pi's comes from agenix, carried over from
# the key headscale generated). Its sha256 is pinned and checked before every
# start, so a key that decrypts to anything else stops headscale instead of
# handing the tailnet a new server identity. A deliberate rotation updates
# both.
{
  config,
  lab,
  lib,
  pkgs,
  ...
}: let
  hs = config.services.headscale;
  db = hs.settings.database.sqlite.path;
  stateDir = "/var/lib/headscale";
  backupDir = "/mnt/headscale-backup";
  sqlite = "${pkgs.sqlite}/bin/sqlite3";
  noiseKey = config.headscaleState.noiseKeyFile;
  noiseKeySha256 = config.headscaleState.noiseKeySha256;

  snapshot = pkgs.writeShellScript "headscale-snapshot" ''
    set -euo pipefail
    ts=$(date -u +%Y%m%dT%H%M%SZ)
    tmp=${backupDir}/.db-$ts.tmp
    ${sqlite} ${db} ".backup '$tmp'"
    [ "$(${sqlite} "$tmp" 'PRAGMA integrity_check')" = ok ]
    mv "$tmp" ${backupDir}/db-$ts.sqlite
    ln -sfn db-$ts.sqlite ${backupDir}/latest.sqlite
    # Ship the local pre-upgrade snapshots too.
    for f in ${stateDir}/pre-upgrade/*.sqlite; do
      [ -e "$f" ] && cp -n "$f" ${backupDir}/
    done
    # Two weeks here; borg on the nas keeps the history.
    ls -1t ${backupDir}/db-*.sqlite | tail -n +15 | xargs -r rm --
  '';

  prepare = pkgs.writeShellScript "headscale-prepare-state" ''
    set -euo pipefail
    version=${hs.package.version}
    if [ "$(sha256sum ${noiseKey} | cut -d' ' -f1)" != ${noiseKeySha256} ]; then
      echo "headscale-prepare-state: ${noiseKey} is not the pinned noise key; not starting headscale with a new identity" >&2
      exit 1
    fi
    if [ ! -e ${db} ]; then
      # Fresh install, or lost state? Only the backups can tell; without
      # them headscale must not start empty.
      if ! timeout 30 ls ${backupDir}/ >/dev/null; then
        echo "headscale-prepare-state: ${db} missing and ${backupDir} unreachable; not starting an empty headscale" >&2
        exit 1
      fi
      if [ -e ${backupDir}/latest.sqlite ]; then
        install -m 0640 -o ${hs.user} -g ${hs.group} "$(readlink -f ${backupDir}/latest.sqlite)" ${db}
        [ "$(${sqlite} ${db} 'PRAGMA integrity_check')" = ok ]
        echo "headscale-prepare-state: restored ${db} from $(readlink -f ${backupDir}/latest.sqlite)"
      else
        echo "headscale-prepare-state: no snapshot on the nas; starting a fresh headscale"
      fi
    elif [ "$(cat ${stateDir}/.nixos-version 2>/dev/null || true)" != "$version" ]; then
      from=$(cat ${stateDir}/.nixos-version 2>/dev/null || echo unknown)
      mkdir -p ${stateDir}/pre-upgrade
      ${sqlite} ${db} ".backup '${stateDir}/pre-upgrade/db-pre-$from-to-$version-$(date -u +%Y%m%dT%H%M%SZ).sqlite'"
      ls -1t ${stateDir}/pre-upgrade/*.sqlite | tail -n +4 | xargs -r rm --
      echo "headscale-prepare-state: snapshot before $from -> $version"
    fi
    echo "$version" > ${stateDir}/.nixos-version
  '';
in {
  imports = [../../role/nfs-automount.nix];

  options.headscaleState = {
    noiseKeyFile = lib.mkOption {
      type = lib.types.str;
      description = "headscale's noise private key, readable by the headscale user.";
    };
    noiseKeySha256 = lib.mkOption {
      type = lib.types.str;
      description = "sha256 of noiseKeyFile; headscale does not start with any other key.";
    };
  };

  config.services.headscale.settings.noise.private_key_path = noiseKey;

  config.nfsAutomounts.${backupDir} = {
    device = "${lab.machines.nas.lan}:/headscale-backup";
    options = ["nfsvers=4.2" "soft" "timeo=100" "retrans=3" "noatime"];
  };

  config.systemd.services.headscale.serviceConfig.ExecStartPre = ["+${prepare}"];

  config.systemd.services.headscale-snapshot = {
    description = "Snapshot headscale's database to the nas";
    serviceConfig = {
      Type = "oneshot";
      ExecStart = snapshot;
    };
  };
  config.systemd.timers.headscale-snapshot = {
    wantedBy = ["timers.target"];
    timerConfig = {
      OnCalendar = "daily";
      Persistent = true;
      RandomizedDelaySec = "30min";
    };
  };
}
