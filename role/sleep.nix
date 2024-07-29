{pkgs, ...}: let
  sleepCheck = pkgs.writeScript "sleepCheck.sh" ''
    STATUS=$(cat /sys/class/power_supply/BAT0/status)
    CAPACITY=$(cat /sys/class/power_supply/BAT0/capacity)
    if [ "''${STATUS}" = "Discharging" ] && [ $CAPACITY -lt 5 ]; then
      systemctl hibernate
    fi
  '';
in {
  services = {
    upower.enable = true;
    cron = {
      enable = true;
      systemCronJobs = ["*/5 * * * * root ${sleepCheck}"];
    };
  };
}
