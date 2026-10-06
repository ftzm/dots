# A transient activation failure: a unit that fails its first start (so the
# switch reports it, exit 4) and succeeds on its own restart seconds later.
{pkgs, ...}: {
  systemd.services.fleet-test-flaky = {
    wantedBy = ["multi-user.target"];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      Restart = "on-failure";
      RestartSec = "5s";
      ExecStart = pkgs.writeShellScript "flaky" ''
        [ -e /run/fleet-test-flaked ] && exit 0
        touch /run/fleet-test-flaked
        exit 1
      '';
    };
  };
}
