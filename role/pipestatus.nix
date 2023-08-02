{ config, pkgs, ... }:

{
  home-manager.users.ftzm = {
    systemd.user.services.pipestatus = {
      Unit = {
        Description = "pipestatus";
        PartOf = [ "graphical-session.target" ];
      };
      Service = {
        ExecStart =
          "${pkgs.pipestatus.pipestatus-wrapped}/bin/pipestatus-wrapped";
        ExecReload = "kill -SIGUSR2 $MAINPID";
        Restart = "on-failure";
        KillMode = "mixed";
        PrivateTmp = "false";
      };
      Install = { WantedBy = [ "graphical-session.target" ]; };
    };

    systemd.user.services.pipestatus_battery = {
      Unit = { Description = "pipestatus battery checker"; };
      Service = {
        Type = "oneshot";
        ExecStart = "${pkgs.pipestatus.scripts}/bin/battery.sh";
      };
    };

    systemd.user.timers.pipestatus_battery = {
      Unit = { Description = "pipestatus battery checker"; };
      Timer = {
        OnBootSec = "1m";
        OnUnitActiveSec = "3m";
        Unit = "pipestatus_battery.service";
      };
      Install = { WantedBy = [ "timers.target" ]; };
    };
  };
}
