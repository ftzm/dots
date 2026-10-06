# Whether each laptop is on the tailnet, as nuc's tailscaled sees it, for
# node_exporter: fleet_tailnet_peer_online{host}. The laptops are excluded
# from the reachability alerts (they sleep), so this is the evidence that a
# laptop is up when its node_exporter is not answering
# (RoamingNodeExporterDown) -- comin's exporter used to be, and it goes with
# comin.
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
    status=$(${config.services.tailscale.package}/bin/tailscale status --json)
    tmp=$(mktemp "${out}.XXXXXX")
    {
      echo "# HELP fleet_tailnet_peer_online 1 while nuc's tailscaled sees the host online."
      echo "# TYPE fleet_tailnet_peer_online gauge"
      ${lib.concatStrings (lib.mapAttrsToList (host: m: ''
        v=$(${pkgs.jq}/bin/jq -r --arg ip ${m.tailscale} '[.Peer[] | select(.TailscaleIPs | index($ip)) | .Online] | if any then 1 else 0 end' <<<"$status")
        echo "fleet_tailnet_peer_online{host=\"${host}\"} $v"
      '')
      laptops)}
    } > "$tmp"
    chmod 0644 "$tmp"
    mv -f "$tmp" ${out}
  '';
in {
  systemd.services.fleet-tailnet-peers = {
    description = "Publish the laptops' tailnet presence for node_exporter";
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
