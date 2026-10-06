# Whether each laptop answers on the tailnet, seen from nuc, for
# node_exporter: fleet_tailnet_peer_online{host}. The laptops are excluded
# from the reachability alerts (they sleep), so this is the evidence that a
# laptop is up when its node_exporter is not answering
# (RoamingNodeExporterDown) -- comin's exporter used to be, and it goes with
# comin.
#
# A ping over the tailnet, not tailscaled's peer `Online` flag: that flag is
# headscale's view of the control session, and headscale 0.28 leaves a node
# marked offline when it reconnects (the old session's disconnect is logged
# after the new session's connect; eachtrai on 2026-10-06, data path up the
# whole time).
{
  config,
  lab,
  lib,
  pkgs,
  ...
}: let
  laptops = lib.filterAttrs (_: m: m ? tailscale && !(m ? lan)) lab.machines;
  out = "${config.nodeExporterTextfileDir}/fleet-tailnet-peers.prom";
  script = pkgs.writeShellScript "fleet-tailnet-peers" ''
    set -eu
    tmp=$(mktemp "${out}.XXXXXX")
    {
      echo "# HELP fleet_tailnet_peer_online 1 while the host answers a ping from nuc over the tailnet."
      echo "# TYPE fleet_tailnet_peer_online gauge"
      ${lib.concatStrings (lib.mapAttrsToList (host: m: ''
        if ${pkgs.iputils}/bin/ping -c 1 -W 2 ${m.tailscale} >/dev/null 2>&1; then v=1; else v=0; fi
        echo "fleet_tailnet_peer_online{host=\"${host}\"} $v"
      '')
      laptops)}
    } > "$tmp"
    chmod 0644 "$tmp"
    mv -f "$tmp" ${out}
  '';
in {
  systemd.services.fleet-tailnet-peers = {
    description = "Publish whether the laptops answer on the tailnet, for node_exporter";
    after = ["tailscaled.service"];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = script;
    };
  };
  systemd.timers.fleet-tailnet-peers = {
    wantedBy = ["timers.target"];
    timerConfig = {
      OnBootSec = "1min";
      OnUnitActiveSec = "1min";
    };
  };
}
