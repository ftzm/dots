{
  config,
  lib,
  ...
}: let
  cfg = config.hm;
in {
  options.hm = lib.mkOption {
    type = lib.types.attrs;
    description = "my home manager options";
  };
  config.home-manager.users.ftzm = cfg;
}
