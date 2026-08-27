# Automounted NFS shares as native systemd units.
#
# Never mount NFS via fileSystems.<path> on comin-deployed machines.
# fileSystems.* writes the mount to /etc/fstab. During a NixOS rebuild switch
# (including switch-to-configuration run by comin), PID 1 reloads its units;
# when the rebuild changes systemd, PID 1 also reexecutes and then runs
# systemd-fstab-generator. The generator canonicalizes each fstab mountpoint
# using stat-based chase(). If the rebuild has just stopped the network path
# to an active hard NFS mount, that stat blocks. The generator child times out,
# and the post-reexec PID 1 freezes:
#
#   systemd[1]: Failed to fork off sandboxing environment for executing generators: Protocol error
#   systemd[1]: Freezing execution.
#
# A frozen PID 1 answers neither D-Bus nor its private socket, so later NixOS
# rebuilds fail with "Failed to subscribe to systemd dbus messages" until a
# forced reboot. Upstream: https://github.com/NixOS/nixpkgs/issues/375376
#
# Native units keep the share out of /etc/fstab, so the generator never stats
# the mountpoint; arming the autofs trigger is a purely local operation.
{
  config,
  lib,
  ...
}: let
  cfg = config.nfsAutomounts;
in {
  options.nfsAutomounts = lib.mkOption {
    default = {};
    description = ''
      NFS shares to mount on demand via native systemd automount units, keyed
      by mountpoint. The share is mounted on first access and unmounted again
      after idleTimeout.
    '';
    type = lib.types.attrsOf (lib.types.submodule {
      options = {
        device = lib.mkOption {
          type = lib.types.str;
          example = "192.0.2.1:/export/share";
          description = "NFS export to mount (server:/path).";
        };
        options = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = ["noatime" "nodiratime"];
          description = "NFS mount options.";
        };
        mountTimeout = lib.mkOption {
          type = lib.types.str;
          default = "15s";
          description = ''
            Timeout for a triggered mount attempt. Bounds how long an
            accessing process blocks when the server is unreachable.
          '';
        };
        idleTimeout = lib.mkOption {
          type = lib.types.str;
          default = "10min";
          description = "Unmount the share after this much idle time.";
        };
      };
    });
  };

  config = {
    systemd.mounts =
      lib.mapAttrsToList (where: share: {
        inherit where;
        what = share.device;
        type = "nfs";
        options = lib.concatStringsSep "," share.options;
        mountConfig.TimeoutSec = share.mountTimeout;
      })
      cfg;

    # Arming the autofs trigger is local; don't gate it on network targets.
    systemd.automounts =
      lib.mapAttrsToList (where: share: {
        inherit where;
        wantedBy = ["multi-user.target"];
        automountConfig.TimeoutIdleSec = share.idleTimeout;
      })
      cfg;
  };
}
