{
  inputs,
  lib,
  ...
}: {
  imports = [inputs.comin.nixosModules.comin];

  # comin installs systems into its own profile, not the system profile, so
  # the deferred-reboot check (role/node-exporter.nix) compares against it.
  nodeExporterDeployProfile = "/nix/var/nix/profiles/system-profiles/comin";

  # comin runs switch-to-configuration as its own child, so the commit that
  # removes comin would have the switch stop comin -- and kill itself midway.
  # With this in the *running* unit, switch-to-configuration leaves a removed
  # comin.service alone (main.rs: removed units are stopped only if their
  # current X-StopOnRemoval is true), and the switch completes; comin is then
  # retired once idle (FORGEJO_MIGRATION_PLAN.md -> Host cut-over).
  systemd.services.comin.unitConfig."X-StopOnRemoval" = false;

  # While comin owns the host, fleet-agent only observes: comin switches to a
  # commit minutes before the writer has published it, and a live agent would
  # switch the host back to the previous manifest meanwhile.
  fleetAgent.dryRun = lib.mkDefault true;

  services.comin = {
    enable = true;
    remotes = [
      {
        name = "origin";
        url = "https://github.com/ftzm/dots.git";
        branches.main.name = "master";
      }
    ];
    # Prometheus exporter — scraped by the cluster (LAN for lab machines,
    # tailscale for laptops) and driving the Comin* alert rules. Default port
    # is 4243; stated explicitly for the scrape config to depend on.
    exporter = {
      port = 4243;
      openFirewall = true; # no-op where networking.firewall.enable = false
    };
  };
}
