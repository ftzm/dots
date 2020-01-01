{ config, pkgs, ... }:

let
  ps = import (builtins.toPath "${config.home.homeDirectory}/dev/pipestatus/release.nix");
in {
  systemd.user.services.pipestatus = {
    Unit = { Description = "pipestatus"; };
    Service = {
      Restart = "always";
      ExecStart = "${ps.pipestatus-wrapped}/bin/pipestatus-wrapped";
      PrivateTmp = "false";
    };
    Install = { WantedBy = [ "graphical-session.target" ]; };
  };

  systemd.user.services.pipestatus_battery = {
    Unit = { Description = "pipestatus battery checker"; };
    Service = {
      Type = "oneshot";
      ExecStart = "${ps.scripts}/bin/battery.sh";
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
