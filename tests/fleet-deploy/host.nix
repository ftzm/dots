# The fleet-deploy test host as the writer builds it: the test node's own
# shape -- VM, test backdoor, its network -- so the driver keeps its shell
# across every switch, plus the real agent role. Called with the node's
# network config (the test repo passes it as JSON) and the role modules (the
# repo's copies, or the dots tree's).
{
  networkConfig,
  fleetAgentModule,
  nodeExporterModule,
}: {modulesPath, ...}: {
  imports = [
    (modulesPath + "/virtualisation/qemu-vm.nix")
    (modulesPath + "/testing/test-instrumentation.nix")
    (modulesPath + "/virtualisation/guest-networking-options.nix")
    networkConfig
    fleetAgentModule
    nodeExporterModule
    ./host-common.nix
  ];
  networking.hostName = "host";
}
