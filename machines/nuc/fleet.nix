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

  # Publishes the manifest for master, read from the nas mirror.
  # nuc deploys itself from its own manifest (no canary, as under comin).
  fleetHost = {
    transport = "lan";
    autoRebootAt = "04:30";
  };

  # The nas mirror, the deploy source of truth (FORGEJO_MIGRATION_PLAN.md ->
  # Decisions), read with a key nas restricts to git-upload-pack.
  age.secrets.fleet-writer-nas-key = {
    file = ../../secrets/fleet-writer-nas-key.age;
    owner = "fleet-writer";
    mode = "0400";
  };

  fleetWriter = {
    enable = true;
    repo = "ssh://git@192.168.1.3/pool-1/git/dots.git";
    sshKeyFile = config.age.secrets.fleet-writer-nas-key.path;
    # nas's host key (secrets/secrets.nix).
    knownHosts = "192.168.1.3 ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFoyzVr7G3uC7YJI4vH8jhYI+sJcIlcckhwzeMVZOYqn";
  };
}
