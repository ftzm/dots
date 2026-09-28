{ config, lib, pkgs, ... }:
let cfg = config.services.agentd;
in {
  options.services.agentd = {
    enable = lib.mkEnableOption "persistent coding-agent user service";
    package = lib.mkOption { type = lib.types.package; default = pkgs.callPackage ./runtime.nix {}; };
    emacsPackage = lib.mkOption {
      type = lib.types.nullOr lib.types.package;
      default = pkgs.callPackage ./emacs.nix {};
    };
  };
  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ cfg.package ] ++ lib.optional (cfg.emacsPackage != null) cfg.emacsPackage;
    systemd.user.services.agentd = {
      description = "Persistent coding agent state";
      wantedBy = [ "default.target" ];
      path = [ cfg.package pkgs.zmx pkgs.coreutils ];
      serviceConfig = {
        ExecStart = "${cfg.package}/bin/agentd";
        Restart = "on-failure";
        RestartSec = 1;
        UMask = "0077";
      };
    };
  };
}
