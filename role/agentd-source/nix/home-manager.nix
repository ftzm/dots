{ config, lib, pkgs, ... }:
let
  cfg = config.services.agentd;
  runtime = pkgs.callPackage ./runtime.nix {};
in {
  options.services.agentd = {
    enable = lib.mkEnableOption "persistent coding-agent state service";
    package = lib.mkOption { type = lib.types.package; default = runtime; description = "Agentd runtime package."; };
    emacsPackage = lib.mkOption {
      type = lib.types.nullOr lib.types.package;
      default = pkgs.callPackage ./emacs.nix {};
      description = "Emacs package to install, or null when installed separately.";
    };
  };
  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.package ] ++ lib.optional (cfg.emacsPackage != null) cfg.emacsPackage;
    systemd.user.services.agentd = {
      Unit = { Description = "Persistent coding agent state"; };
      Service = {
        ExecStart = "${cfg.package}/bin/agentd";
        Restart = "on-failure";
        RestartSec = 1;
        UMask = "0077";
        # Do not remove this runtime directory: zmx sessions outlive agentd.
        Environment = [ "PATH=${lib.makeBinPath [ cfg.package pkgs.zmx pkgs.coreutils ]}:%h/.nix-profile/bin:/run/current-system/sw/bin" ];
      };
      Install.WantedBy = [ "default.target" ];
    };
  };
}
