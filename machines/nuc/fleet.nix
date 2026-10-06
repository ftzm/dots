# nuc as the fleet's deploy plane (FORGEJO_MIGRATION_PLAN.md -> Binary Cache).
# The cache is live: harmonia serves nuc's store signed with the cache key,
# and nix signs what nuc builds with it (verified: harmonia's narinfo
# signatures check against nuc-fleet-1). The hosts' agents come next.
{config, ...}: {
  imports = [../../role/fleet-writer.nix];

  age.secrets.fleet-cache-key = {
    file = ../../secrets/fleet-cache-key.age;
    mode = "0400";
  };

  fleetCache = {
    enable = true;
    signKeyFile = config.age.secrets.fleet-cache-key.path;
  };

  # Publishes the manifest for master. Reads GitHub until the flip, then the
  # nas mirror (lab.services.nasMirror, with a read-only key).
  fleetWriter = {
    enable = true;
    repo = "https://github.com/ftzm/dots.git";
  };
}
