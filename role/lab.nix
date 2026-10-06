# Lab network topology: machine IPs and service endpoints.
# Imported by lab machines (nuc, nas) to bootstrap inter-machine
# connections before DNS consolidation (Phase 5), and by every host that runs
# fleet-agent.
{...}: let
  machines = {
    nuc = {
      lan = "192.168.1.4";
      wg = "10.0.100.4";
      tailscale = "100.64.0.2";
    };
    nas = {
      lan = "192.168.1.3";
      wg = "10.0.100.3";
    };
    pi = {
      lan = "192.168.1.12";
    };
    # The laptops, by tailnet address (as in cluster/lib/config.libsonnet;
    # eachtrai's live tailnet node is `eachtrai-5k2mrtvr`).
    saoiste = {
      tailscale = "100.64.0.1";
    };
    eachtrai = {
      tailscale = "100.64.0.7";
    };
  };

  # Internal service endpoints — direct IP:port, no DNS needed
  services = {
    lokiPush = "http://${machines.nuc.lan}:30100/loki/api/v1/push";

    # The deploy plane on nuc (FORGEJO_MIGRATION_PLAN.md -> Binary Cache):
    # the manifest pointer (nginx /fleet/) and the binary cache (harmonia),
    # by LAN for the lab hosts and by Tailscale for the laptops. Nothing
    # dials wg (INCIDENT-2026-08-wireguard-transport.md).
    fleetManifestLan = "http://${machines.nuc.lan}:5001/fleet/manifest";
    fleetManifestTailscale = "http://${machines.nuc.tailscale}:5001/fleet/manifest";
    fleetCacheLan = "http://${machines.nuc.lan}:5000";
    fleetCacheTailscale = "http://${machines.nuc.tailscale}:5000";
    # The deploy source of truth: a bare repo on nas, fed by Forgejo's push
    # mirror; ArgoCD and the fleet writer read it.
    nasMirror = "ssh://git@${machines.nas.lan}/dots.git";
    claudeProxy = "http://${machines.nuc.lan}:5002";
  };
in {
  _module.args.lab = {
    inherit machines services;
  };
}
