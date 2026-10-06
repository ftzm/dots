# checks.x86_64-linux.fleet-deploy: the real fleet writer, harmonia and the
# manifest pointer on `nuc`, the real fleet-agent role on `host`
# (FORGEJO_MIGRATION_PLAN.md -> Binary Cache -> Deploy test).
#
# A bare repo on nuc stands in for the nas mirror. Its commits are a small
# flake whose `fleet` linkFarm holds one system, `host`, built from
# host-common.nix plus one file of variants/. nuc has no substituters and no
# build dependencies: every variant's system is built here, outside, from the
# very same files and nixpkgs source, and put in nuc's store, so the writer's
# `nix build` inside the VM evaluates to paths that are already valid -- as on
# the real nuc when CI built them before the merge. A variant that fails to
# build fails there too.
#
# The cache key is a test-only key generated for this test.
{
  pkgs,
  fleetAgentModule,
  fleetWriterModule,
  nodeExporterModule,
}: let
  inherit (pkgs) lib;
  system = "x86_64-linux";

  # The nixpkgs source under the name a flake `path:` input gets, so the
  # inner flake's input is this very store path.
  nixpkgsSrc = builtins.path {
    path = pkgs.path;
    name = "source";
  };

  # Everything the test repo holds but the variant.
  innerSrc = networkConfig:
    pkgs.runCommand "fleet-deploy-repo" {} ''
      mkdir $out
      cp ${fleetAgentModule} $out/fleet-agent.nix
      cp ${nodeExporterModule} $out/node-exporter.nix
      cp ${./host-common.nix} $out/host-common.nix
      cp ${./test-cache-key.pub} $out/test-cache-key.pub
      cp ${pkgs.writeText "network.json" (builtins.toJSON networkConfig)} $out/network.json
      cp ${./host.nix} $out/host.nix
      cp ${pkgs.writeText "flake.nix" ''
        {
          inputs.nixpkgs = {
            url = "path:${nixpkgsSrc}";
            flake = false;
          };
          outputs = {nixpkgs, ...}: let
            host = import "''${nixpkgs}/nixos/lib/eval-config.nix" {
              system = "${system}";
              modules = [
                (import ./host.nix {
                  networkConfig = builtins.fromJSON (builtins.readFile ./network.json);
                  fleetAgentModule = ./fleet-agent.nix;
                  nodeExporterModule = ./node-exporter.nix;
                })
                ./variant.nix
              ];
            };
          in {
            packages.${system}.fleet =
              (import nixpkgs {system = "${system}";}).linkFarm "fleet"
              {host = host.config.system.build.toplevel;};
          };
        }
      ''} $out/flake.nix
      echo "fleet-deploy test repo" > $out/README
    '';

  variantNames = ["base" "changed" "agent-change" "flaky" "hang" "dbus" "interface" "observe"];

  # What the writer will find already built: each variant's system and its
  # fleet linkFarm, evaluated exactly as the inner flake evaluates them.
  outer = networkConfig: let
    src = innerSrc networkConfig;
    hostSystem = v:
      (import "${nixpkgsSrc}/nixos/lib/eval-config.nix" {
        inherit system;
        modules = [
          (import ./host.nix {inherit networkConfig fleetAgentModule nodeExporterModule;})
          ./variants/${v}.nix
        ];
      }).config.system.build.toplevel;
    fleet = v: (import nixpkgsSrc {inherit system;}).linkFarm "fleet" {host = hostSystem v;};
  in {
    inherit src;
    systems = lib.genAttrs variantNames hostSystem;
    fleets = lib.genAttrs variantNames fleet;
  };
in
  pkgs.testers.runNixOSTest ({nodes, ...}: let
    o = outer nodes.host.system.build.networkConfig;
  in {
    name = "fleet-deploy";

    nodes.host.imports = [fleetAgentModule nodeExporterModule ./host-common.nix];

    nodes.nuc = {
      imports = [fleetWriterModule nodeExporterModule];
      fleetCache = {
        enable = true;
        signKeyFile = "${./test-cache-key.sec}";
      };
      fleetWriter = {
        enable = true;
        repo = "file:///srv/dots.git";
      };
      nix.settings = {
        experimental-features = ["nix-command" "flakes"];
        substituters = lib.mkForce [];
      };
      environment.systemPackages = [pkgs.git pkgs.jq];
      virtualisation = {
        memorySize = 4096;
        cores = 4;
        diskSize = 8192;
        additionalPaths =
          [nixpkgsSrc o.src]
          ++ lib.attrValues o.systems
          ++ lib.attrValues o.fleets;
      };
    };

    testScript = ''
      import json

      expected = {
      ${lib.concatMapStringsSep "\n" (v: ''"${v}": "${o.systems.${v}}",'') variantNames}
      }

      start_all()
      nuc.wait_for_unit("multi-user.target")
      nuc.wait_for_unit("harmonia.socket")
      nuc.wait_for_unit("nginx.service")
      host.wait_for_unit("multi-user.target")

      def commit(msg, variant=None, readme=None):
          if variant is not None:
              nuc.succeed(f"cp ${./variants}/{variant}.nix /root/work/variant.nix")
          if readme is not None:
              nuc.succeed(f"echo '{readme}' >> /root/work/README")
          nuc.succeed(
              "cd /root/work && git add -A"
              f" && git commit -q -m '{msg}'"
              " && git push -q /srv/dots.git HEAD:master"
              " && chown -R fleet-writer:fleet-writer /srv/dots.git"
          )
          return nuc.succeed("git -C /root/work rev-parse HEAD").strip()

      def write():
          # oneshot: returns when the run ends; the exit status is the run's.
          return nuc.execute("systemctl start fleet-writer.service")[0]

      def manifest():
          ptr = nuc.succeed("cat /var/www/fleet/manifest").strip()
          return ptr, json.loads(nuc.succeed(f"cat {ptr}"))

      def writer_metric(name):
          return nuc.succeed(
              f"awk '$1 ~ /^{name}/ {{print $2}}' /var/lib/prometheus-node-exporter-text-files/fleet-writer.prom"
          ).strip()

      def agent():
          return host.execute("systemctl start fleet-agent.service")[0]

      def agent_metrics():
          return host.succeed("cat /var/lib/prometheus-node-exporter-text-files/fleet-agent.prom")

      def current():
          return host.succeed("readlink -f /run/current-system").strip()

      def reboot_reason():
          host.succeed("systemctl start nixos-reboot-required-metrics.service")
          prom = host.succeed("cat /var/lib/prometheus-node-exporter-text-files/nixos-reboot-required.prom")
          line = [l for l in prom.splitlines() if l.startswith("nixos_reboot_required{")][0]
          return line.split('reason="')[1].split('"')[0], line.rsplit(" ", 1)[1]

      # Packets nuc received on harmonia's port (a counting rule, no target).
      def harmonia_requests():
          return int(nuc.succeed("iptables -L INPUT -v -x -n | awk '/dpt:5000/ {print $1; exit}'").strip())

      def reboot():
          host.shutdown()
          host.start()
          host.wait_for_unit("multi-user.target")

      nuc.succeed("iptables -I INPUT -p tcp --dport 5000")

      with subtest("set up the deploy source"):
          nuc.succeed(
              "mkdir -p /root/work && cp -r ${o.src}/. /root/work && chmod -R u+w /root/work"
              " && git -C /root/work init -q -b master"
              " && git -C /root/work config user.email test@test && git -C /root/work config user.name test"
              " && git init -q --bare -b master /srv/dots.git"
          )
          # root pushes into the bare repo fleet-writer owns.
          nuc.succeed("git config --global --add safe.directory /srv/dots.git")
          nuc.succeed("cp ${./variants}/base.nix /root/work/variant.nix")
          nuc.succeed("cd /root/work && git add -A && nix flake lock")

      with subtest("first publish: the host substitutes only what it lacks and switches"):
          c1 = commit("base", variant="base")
          assert write() == 0, "writer failed on base"
          ptr1, m1 = manifest()
          assert m1["commit"] == c1, m1
          assert m1["hosts"]["host"]["path"] == expected["base"], (m1, expected["base"])
          assert m1["hosts"]["host"]["commit"] == c1, m1
          before = harmonia_requests()
          assert agent() == 0
          assert current() == expected["base"]
          # The counter sees traffic: the check below is not vacuous.
          assert harmonia_requests() > before
          out = host.succeed("journalctl -u fleet-agent.service -o cat")
          assert "building" not in out, out
          assert f'fleet_deployed_commit_info{{commit="{c1}"}} 1' in agent_metrics()
          assert "fleet_last_failure 0" in agent_metrics()
          assert writer_metric("fleet_writer_last_failure") == "0"

      with subtest("an unchanged host makes no request beyond the manifest"):
          before = harmonia_requests()
          assert agent() == 0
          assert harmonia_requests() == before, (before, harmonia_requests())

      with subtest("a README-only commit publishes nothing"):
          commit("readme", readme="more")
          assert write() == 0
          ptr, _ = manifest()
          assert ptr == ptr1, (ptr, ptr1)

      with subtest("a changed host follows"):
          c3 = commit("changed", variant="changed")
          assert write() == 0
          _, m3 = manifest()
          assert m3["hosts"]["host"]["path"] == expected["changed"]
          assert agent() == 0
          assert current() == expected["changed"]
          host.succeed("grep -q changed /etc/fleet-test")

      with subtest("a revert republishes the older path and the host follows"):
          c4 = commit("revert", variant="base")
          assert write() == 0
          _, m4 = manifest()
          assert m4["hosts"]["host"]["path"] == expected["base"]
          assert m4["commit"] == c4
          assert agent() == 0
          assert current() == expected["base"]

      with subtest("a commit that fails to build publishes nothing"):
          ptr_before, _ = manifest()
          commit("broken", variant="failing")
          assert write() != 0
          assert manifest()[0] == ptr_before
          assert writer_metric("fleet_writer_last_failure") == "1"
          # The failed head is not rebuilt on the next tick.
          assert write() != 0
          assert "not retrying yet" in nuc.succeed("journalctl -u fleet-writer.service -n 5 -o cat")

      with subtest("a commit changing fleet-agent.service completes its own switch"):
          commit("agent change", variant="agent-change")
          # Past the failed head's back-off.
          nuc.succeed("touch -d '-11 min' /var/lib/fleet-writer/failed-head")
          assert write() == 0
          assert writer_metric("fleet_writer_last_failure") == "0"
          assert agent() == 0
          assert current() == expected["agent-change"]
          host.succeed("systemctl show -p Environment fleet-agent.service | grep -q FLEET_TEST=agent-change")

      with subtest("a transient activation failure is retried once and clears"):
          commit("flaky", variant="flaky")
          assert write() == 0
          assert agent() != 0
          assert current() == expected["flaky"]
          assert "fleet_last_failure 1" in agent_metrics()
          # Not before 30 minutes.
          assert agent() == 0
          assert "fleet_last_failure 1" in agent_metrics()
          host.wait_until_succeeds("systemctl is-active fleet-test-flaky.service")
          host.succeed("touch -d '-31 min' /var/lib/fleet-agent/failed")
          assert agent() == 0
          assert "fleet_last_failure 0" in agent_metrics()
          host.succeed("test ! -e /var/lib/fleet-agent/failed")

      with subtest("a hung activation is bounded and reported; booting it clears the flag"):
          commit("hang", variant="hang")
          assert write() == 0
          assert agent() != 0
          assert "fleet_last_failure 1" in agent_metrics()
          assert current() == expected["hang"]
          host.succeed("test \"$(cat /var/lib/fleet-agent/failed)\" = ${o.systems.hang}")
          # Not left blocked, and not retried: the next run only reads the manifest.
          host.succeed("! systemctl is-active fleet-agent.service")
          before = harmonia_requests()
          assert agent() == 0
          assert harmonia_requests() == before
          assert "fleet_last_failure 1" in agent_metrics()
          # Retried once after 30 minutes, which hangs again; then never again.
          host.succeed("touch -d '-31 min' /var/lib/fleet-agent/failed")
          assert agent() != 0
          host.succeed("journalctl -u fleet-agent.service -o cat | grep -q 'retrying the failed activation'")
          host.succeed("touch -d '-31 min' /var/lib/fleet-agent/failed")
          before = harmonia_requests()
          assert agent() == 0
          assert harmonia_requests() == before
          assert "fleet_last_failure 1" in agent_metrics()
          host.succeed("touch /var/lib/fleet-test-no-hang")
          reboot()
          assert agent() == 0
          assert "fleet_last_failure 0" in agent_metrics()
          host.succeed("test ! -e /var/lib/fleet-agent/failed")

      with subtest("a changed switch inhibitor installs the boot entry and defers"):
          c7 = commit("dbus", variant="dbus")
          assert write() == 0
          old = current()
          assert agent() == 0
          assert current() == old
          assert host.succeed("readlink -f /nix/var/nix/profiles/system").strip() == expected["dbus"]
          assert reboot_reason() == ("deferred", "1")
          assert "fleet_last_failure 0" in agent_metrics()
          assert f'fleet_deployed_commit_info{{commit="{c7}"}} 1' in agent_metrics()
          before = harmonia_requests()
          assert agent() == 0
          assert harmonia_requests() == before

      with subtest("autoReboot reboots into the deferred system, once"):
          host.succeed("systemctl start --no-block fleet-agent-reboot.service")
          host.wait_for_shutdown()
          host.start()
          host.wait_for_unit("multi-user.target")
          assert current() == expected["dbus"]
          assert reboot_reason() == ("", "0")
          # Nothing pending now: the window passes without a reboot.
          host.succeed("systemctl start fleet-agent-reboot.service")
          host.succeed("true")

      with subtest("a systemd whose interface version differs defers (switch exits 100)"):
          commit("interface", variant="interface")
          assert write() == 0
          old = current()
          assert agent() == 0
          assert current() == old
          assert host.succeed("readlink -f /nix/var/nix/profiles/system").strip() == expected["interface"]
          assert reboot_reason() == ("deferred", "1")
          assert "fleet_last_failure 0" in agent_metrics()
          assert "cannot be switched to live" in host.succeed("journalctl -u fleet-agent.service -o cat")
          # A system that does not come up as itself is not rebooted into twice.
          host.succeed("echo ${o.systems.interface} > /var/lib/fleet-agent/rebooted-for")
          host.fail("systemctl start fleet-agent-reboot.service")
          host.succeed("journalctl -u fleet-agent-reboot.service -o cat | grep -q 'already rebooted once'")
          host.succeed("rm /var/lib/fleet-agent/rebooted-for")
          reboot()
          assert current() == expected["interface"]
          assert reboot_reason() == ("", "0")

      with subtest("dryRun downloads the published system but activates nothing"):
          commit("observe", variant="observe")
          assert write() == 0
          # The live agent installs the observing system (deferred: the host
          # runs the interface variant, whose dbus and systemd differ), and the
          # host boots it...
          assert agent() == 0
          reboot()
          assert current() == expected["observe"]
          # ...whose agent then only fetches what the next commit names.
          c = commit("changed again", variant="changed")
          assert write() == 0
          assert agent() == 0
          assert current() == expected["observe"]
          host.succeed("nix-store --check-validity ${o.systems.changed}")
          host.succeed("journalctl -u fleet-agent.service -o cat | grep -q 'dry run -- would deploy ${o.systems.changed}'")
          assert f'fleet_deployed_commit_info{{commit="{c}"}}' not in agent_metrics()

      with subtest("GC keeps both published generations"):
          nuc.succeed("nix-store --gc")
          gens = nuc.succeed("ls /nix/var/nix/profiles/per-user/fleet-writer/").split()
          assert len([g for g in gens if g.startswith("fleet-") and g.endswith("-link") and "manifest" not in g]) == 2, gens
          for v in ["observe", "changed"]:
              nuc.succeed(f"nix-store --check-validity {expected[v]}")
    '';
  })
