# Automatic recovery from a wedged init or a crashed kernel.
#
# Written after saoiste had to be power-cycled on 2026-07-30. Two independent
# failures, five days apart, and neither one announced itself:
#
#   Jul 25 06:31  PID 1 froze partway through a comin `switch-to-configuration`.
#                 systemd re-executed itself, failed to set up the sandbox it
#                 runs unit generators in, and responded by freezing forever:
#
#                   systemd[1]: Failed to fork off sandboxing environment for
#                               executing generators: Protocol error
#                   systemd[1]: Freezing execution.
#
#                 The generator sandbox has a 90s deadline and the box was busy
#                 with a nix build, so it missed it (systemd/systemd#39354).
#                 saoiste then ran 5d14h with no service management, no socket
#                 activation and no coredump capture — PID 1 logged not one
#                 line in that window — while comin went on reporting deploys
#                 as successful. Nobody noticed until the machine died.
#
#   Jul 30 20:37  Kernel panic: runaway nested int3 exceptions overflowed the
#                 exception stack into its guard page, double-faulted, and the
#                 oops handler double-faulted again writing the crash log
#                 ("Kernel panic - not syncing: Fatal exception in interrupt").
#                 kernel.panic was 0, so the box hung at a dead console instead
#                 of rebooting, and needed a physical power cycle.
#
# The watchdog is the load-bearing part here. Everything else in the system
# that could have raised a hand — timers, service restarts, monitoring — runs
# *under* PID 1, so all of it died with PID 1 on Jul 25. A hardware watchdog is
# the only mechanism that outlives init.
{...}: {
  systemd.settings.Manager = {
    # systemd pets /dev/watchdog from PID 1's main event loop, so a frozen or
    # panicked PID 1 stops petting and the chipset resets the board. This is
    # what turns the Jul 25 failure from five days of silent rot into a
    # two-minute outage.
    #
    # 2min is deliberately slack. PID 1 is mlocked and runs at high priority,
    # so missing two minutes of pings means genuinely wedged, not merely slow
    # under a 24-thread nix build. Machines with no watchdog device just log a
    # warning at boot and carry on.
    RuntimeWatchdogSec = "2min";

    # Bound a hung shutdown too — a reboot that stalls on some unit's stop job
    # is the same "needs a power cycle" outcome by another route.
    RebootWatchdogSec = "10min";
  };

  boot.kernel.sysctl = {
    # Reboot 30s after a panic instead of sitting at a dead console forever.
    # The default of 0 means "hang", which is what forced the Jul 30 power
    # cycle. 30s leaves time to photograph the trace off a monitor; the full
    # log is recovered from EFI pstore on the next boot regardless.
    "kernel.panic" = 30;

    # Treat any oops as fatal rather than limping on. An oopsed kernel has
    # already left something in an undefined state — commonly a lock held by a
    # task that just died, which presents to the user as "SSH stopped
    # responding" rather than as a crash. Better to take the reboot and get a
    # pstore trace out of it.
    #
    # Set to 0 if a survivable driver oops ever starts costing unsaved work.
    "kernel.panic_on_oops" = 1;
  };
}
