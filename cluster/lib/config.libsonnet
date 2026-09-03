// Machine inventory — single source of truth for scrape targets and
// topology. The Nix side keeps its own copy in role/lab.nix; crossing the
// nix/jsonnet boundary isn't worth it.
local machines = {
  nuc: { lan: '192.168.1.4', wg: '10.0.100.4', tailscale: '100.64.0.2' },
  nas: { lan: '192.168.1.3' },
  saoiste: { tailscale: '100.64.0.1' },
  // Live tailnet node is `eachtrai-5k2mrtvr` (the bare `eachtrai` and two
  // stale registrations are offline — see INCIDENT-2026-08-wireguard-transport.md).
  // Re-check after the tailnet cleanup: a re-registration changes this IP.
  eachtrai: { tailscale: '100.64.0.7' },
};

{
  nasIP: machines.nas.lan,
  publicIP: machines.nuc.lan,
  tailscaleIP: machines.nuc.tailscale,
  wgIP: machines.nuc.wg,
  machines: machines,
}
