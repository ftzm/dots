# checks.x86_64-linux.headscale-state: machines/pi/headscale-state.nix with a
# real headscale and an NFS server standing in for the nas.
{
  pkgs,
  headscaleStateModule,
}:
pkgs.testers.runNixOSTest ({nodes, ...}: {
  name = "headscale-state";

  nodes.nas = {
    services.nfs.server = {
      enable = true;
      exports = ''
        /pool-1/ *(rw,fsid=root,no_subtree_check,no_root_squash)
      '';
    };
    systemd.tmpfiles.rules = ["d /pool-1/headscale-backup 0700 root root -"];
    networking.firewall.enable = false;
    environment.systemPackages = [pkgs.sqlite];
  };

  nodes.headscale = {
    imports = [headscaleStateModule];
    _module.args.lab.machines.nas.lan = nodes.nas.networking.primaryIPAddress;
    services.headscale = {
      enable = true;
      settings = {
        server_url = "http://headscale:8080";
        dns = {
          base_domain = "tail.example";
          override_local_dns = false;
        };
        # No internet in the test: headscale's embedded DERP server instead of
        # the public DERP map it would fetch at startup.
        derp = {
          urls = [];
          server = {
            enabled = true;
            region_id = 999;
            stun_listen_addr = "0.0.0.0:3478";
          };
        };
      };
    };
    environment.systemPackages = [pkgs.sqlite];
  };

  testScript = ''
    db = "/var/lib/headscale/db.sqlite"

    def users():
        return headscale.succeed("headscale users list -o json")

    start_all()
    nas.wait_for_unit("nfs-server.service")

    with subtest("a fresh install with no snapshot starts empty"):
        headscale.wait_for_unit("headscale.service")
        headscale.wait_for_open_port(8080)
        headscale.succeed("journalctl -u headscale -o cat | grep -q 'no snapshot on the nas; starting a fresh headscale'")
        headscale.succeed("headscale users create alice")

    with subtest("the daily snapshot lands on the nas"):
        headscale.succeed("systemctl start headscale-snapshot.service")
        nas.succeed("test -e /pool-1/headscale-backup/latest.sqlite")
        nas.succeed("sqlite3 \"$(readlink -f /pool-1/headscale-backup/latest.sqlite)\" 'select name from users' | grep -qx alice")

    with subtest("a lost database is restored before headscale starts"):
        headscale.succeed("systemctl stop headscale")
        headscale.succeed(f"rm -f {db} {db}-wal {db}-shm")
        headscale.succeed("systemctl start headscale")
        headscale.wait_for_open_port(8080)
        assert "alice" in users(), users()
        headscale.succeed("journalctl -u headscale -o cat | grep -q 'restored /var/lib/headscale/db.sqlite'")

    with subtest("with the nas unreachable a lost database does not start empty"):
        headscale.succeed("systemctl stop headscale")
        headscale.succeed(f"rm -f {db} {db}-wal {db}-shm")
        nas.succeed("systemctl stop nfs-server")
        headscale.succeed("umount -l /mnt/headscale-backup || true")
        headscale.fail("systemctl start headscale")
        headscale.succeed(f"test ! -e {db}")
        headscale.succeed("journalctl -u headscale -o cat | grep -q 'unreachable; not starting an empty headscale'")
        # It keeps retrying (Restart=always) and recovers once the nas is back.
        nas.succeed("systemctl start nfs-server")
        headscale.wait_until_succeeds("headscale users list -o json | grep -q alice", timeout=120)

    with subtest("a version change snapshots before the migration"):
        headscale.succeed("systemctl stop headscale")
        headscale.succeed("echo 0.0.1 > /var/lib/headscale/.nixos-version")
        headscale.succeed("systemctl start headscale")
        headscale.wait_for_open_port(8080)
        headscale.succeed("ls /var/lib/headscale/pre-upgrade/db-pre-0.0.1-to-*.sqlite")
        headscale.succeed("systemctl start headscale-snapshot.service")
        nas.succeed("ls /pool-1/headscale-backup/db-pre-0.0.1-to-*.sqlite")
  '';
})
