# Transitional, for the cut-over to fleet-agent (FORGEJO_MIGRATION_PLAN.md ->
# Host cut-over): comin evaluates `config.services.comin.machineId` of the
# configuration it is about to deploy and refuses one that lacks the option
# (it refused commit 4b0b1826 that way). So the commit comin applies to
# retire itself still imports its module, disabled. Removed, with the comin
# flake input, once fleet-agent deploys every host.
{inputs, ...}: {
  imports = [inputs.comin.nixosModules.comin];
  services.comin.enable = false;
}
