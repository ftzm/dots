# The fleet-deploy test's host, shared by the test node and every system the
# writer builds for it from the test repo: the real agent role, nuc as its
# only cache, a real bootloader so a reboot boots the installed entry.
# The role modules are imported next to it: from the dots tree by the test
# node, from the test repo's copies by the systems the writer builds.
{lib, ...}: {
  fleetAgent = {
    enable = true;
    # nuc is test node 2 (nodes are numbered alphabetically: host, nuc).
    manifestUrl = "http://192.168.1.2:5001/fleet/manifest";
    cacheUrl = "http://192.168.1.2:5000";
    cachePublicKey = lib.fileContents ./test-cache-key.pub;
    activationTimeout = "120s";
    # The test waits out the grace for a foreign activation.
    foreignGraceSeconds = 20;
    # The test starts the reboot service itself.
    autoReboot = {
      enable = true;
      at = "*-01-01 04:00";
    };
  };
  # The test drives every run; the timer must not race it.
  systemd.timers.fleet-agent.timerConfig.OnBootSec = lib.mkForce "1d";
  nix.settings.substituters = lib.mkForce ["http://192.168.1.2:5000"];

  system.switch.enable = true;
  virtualisation = {
    useBootLoader = true;
    useEFIBoot = true;
    memorySize = 2048;
    # Room for a few systems above min-free, so no auto-GC muddies the runs:
    # the writable store overlay on this disk, not on a RAM-sized tmpfs.
    diskSize = 8192;
    writableStoreUseTmpfs = false;
  };
  boot.loader.systemd-boot.enable = true;
  # The boot-loader disk is a prebuilt image whose root partition keeps the
  # image's size; grow it into the disk.
  boot.growPartition = true;
  virtualisation.fileSystems."/".autoResize = true;
  documentation.enable = false;
}
