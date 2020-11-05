{ config, pkgs, ... }:

{
  systemd.user.services.pipestatus = {
    Unit = { Description = "pipestatus"; };
    Service = {
      Restart = "always";
      ExecStart = "${pkgs.pipestatus.pipestatus-wrapped}/bin/pipestatus-wrapped";
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
}
