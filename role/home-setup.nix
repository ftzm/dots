{
  lib,
  ...
}: {
  # Create "hm" as a module-level alias for "home-manager.users.ftzm".
  # Unlike a naive passthrough (e.g. config.home-manager.users.ftzm = config.hm),
  # mkAliasOptionModule routes raw definitions through the module system,
  # preserving priority wrappers (mkDefault, mkOptionDefault, mkForce) and
  # deep recursive merging at arbitrary depth across multiple modules.
  imports = [
    (lib.mkAliasOptionModule ["hm"] ["home-manager" "users" "ftzm"])
  ];
}
