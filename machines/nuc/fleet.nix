# nuc as the fleet's deploy plane (FORGEJO_MIGRATION_PLAN.md -> Binary Cache).
# The cache is live: harmonia serves nuc's store signed with the cache key,
# and nix signs what nuc builds with it (verified: harmonia's narinfo
# signatures check against nuc-fleet-1). The writer (fleetWriter) and the
# hosts' agents come next.
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
}
