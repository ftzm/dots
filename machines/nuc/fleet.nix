# nuc as the fleet's deploy plane (FORGEJO_MIGRATION_PLAN.md -> Binary Cache).
# The cache is live: harmonia serves nuc's store signed with the cache key,
# and nix signs what nuc builds with it. The writer (fleetWriter) and the
# hosts' agents come next.
{
  config,
  lib,
  ...
}: {
  imports = [../../role/fleet-writer.nix];

  age.secrets.fleet-cache-key = {
    file = ../../secrets/fleet-cache-key.age;
    mode = "0400";
  };

  fleetCache = {
    enable = true;
    signKeyFile = config.age.secrets.fleet-cache-key.path;
  };
  # Until harmonia's signatures prove the key decrypts on nuc: a bad key here
  # would fail every build nuc's daemon runs, comin's own included.
  nix.settings.secret-key-files = lib.mkForce [];
}
