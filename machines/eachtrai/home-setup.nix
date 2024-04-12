{
  config,
  lib,
  ...
}: let
  cfg = config.my-home;
in {
  options.my-home = lib.mkOption {
    type = lib.types.attrs;
    description = "my home options";
  };
  config.home-manager.users.ftzm = cfg;
}
