# A commit whose system does not build.
{
  system.extraDependencies = [
    (derivation {
      name = "fleet-test-broken";
      system = "x86_64-linux";
      builder = "/bin/sh";
      args = ["-c" "exit 1"];
    })
  ];
}
