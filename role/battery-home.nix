{pkgs, ...}: let
  batteryScript = pkgs.writeShellScript "battery-monitor" ''
    BAT=/sys/class/power_supply/BAT0
    STATE_FILE="''${XDG_RUNTIME_DIR:-/tmp}/battery-monitor-state"

    if [ ! -d "$BAT" ]; then
      exit 0
    fi

    STATUS=$(cat "$BAT/status")
    CAPACITY=$(cat "$BAT/capacity")

    # Reset state when charging
    if [ "$STATUS" != "Discharging" ]; then
      rm -f "$STATE_FILE"
      exit 0
    fi

    LAST_LEVEL=$(cat "$STATE_FILE" 2>/dev/null || echo "none")

    if [ "$CAPACITY" -le 5 ]; then
      ${pkgs.libnotify}/bin/notify-send -u critical "Battery Critical" "Battery at $CAPACITY%. Hibernating now."
      sleep 5
      systemctl hibernate
    elif [ "$CAPACITY" -le 10 ] && [ "$LAST_LEVEL" != "critical" ]; then
      ${pkgs.libnotify}/bin/notify-send -u critical "Battery Critical" "Battery at $CAPACITY%. Plug in immediately!"
      echo "critical" > "$STATE_FILE"
    elif [ "$CAPACITY" -le 20 ] && [ "$LAST_LEVEL" != "low" ] && [ "$LAST_LEVEL" != "critical" ]; then
      ${pkgs.libnotify}/bin/notify-send -u normal "Battery Low" "Battery at $CAPACITY%."
      echo "low" > "$STATE_FILE"
    fi
  '';
in {
  systemd.user.services.battery-monitor = {
    Unit.Description = "Battery level monitor";
    Service = {
      Type = "oneshot";
      ExecStart = "${batteryScript}";
    };
  };

  systemd.user.timers.battery-monitor = {
    Unit.Description = "Check battery level periodically";
    Timer = {
      OnBootSec = "1min";
      OnUnitActiveSec = "2min";
    };
    Install.WantedBy = ["timers.target"];
  };
}
