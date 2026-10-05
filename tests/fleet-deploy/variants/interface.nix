# dbus as in dbus.nix (so no inhibitor differs from it), plus a systemd whose
# interface version differs from the running PID 1's: `switch` exits 100.
# passthru is not part of the derivation, so nothing rebuilds.
{pkgs, ...}: {
  services.dbus.implementation = "dbus";
  systemd.package = pkgs.systemd.overrideAttrs (o: {
    passthru = o.passthru // {interfaceVersion = 99;};
  });
}
