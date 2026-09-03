{inputs, ...}: {
  imports = [inputs.comin.nixosModules.comin];

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
