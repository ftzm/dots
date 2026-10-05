# An activation that hangs in the unit phase, after /run/current-system is
# linked: a oneshot the switch starts and waits on, past the agent's bound.
# The test creates the condition file before rebooting into this system, so
# the boot itself does not wait on the unit (multi-user.target is ordered
# after what it wants).
{pkgs, ...}: {
  systemd.services.fleet-test-hang = {
    wantedBy = ["multi-user.target"];
    unitConfig.ConditionPathExists = "!/var/lib/fleet-test-no-hang";
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${pkgs.coreutils}/bin/sleep 600";
    };
  };
}
