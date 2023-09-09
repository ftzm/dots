{ lib, config, ... }:
let cfg = config.home-conf;
in {
  options.home-conf = lib.mkOption {
    type = lib.types.attrs;
    # description = "Base font size.";
  };
  config.home-manager.users.ftzm = cfg;
}
