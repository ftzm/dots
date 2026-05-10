# Lab network topology: machine IPs and service endpoints.
# Imported by lab machines (nuc, nas) to bootstrap inter-machine
# connections before DNS consolidation (Phase 5).
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
  };

  # Internal service endpoints — direct IP:port, no DNS needed
  services = {
    lokiPush = "http://${machines.nuc.wg}:30100/loki/api/v1/push";
  };
in {
  _module.args.lab = {
    inherit machines services;
  };
}
