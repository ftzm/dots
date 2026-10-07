# The OpenTofu provider for Forgejo (FORGEJO_MIGRATION_PLAN.md -> Secrets ->
# Forgejo configuration as OpenTofu); not in nixpkgs' terraform-providers.
# Imports nothing from role/ or machines/: nix-update evaluates it impurely
# inside the Renovate job, which keeps `version`, `hash` and `vendorHash`
# current.
{terraform-providers}:
terraform-providers.mkProvider {
  owner = "svalabs";
  repo = "terraform-provider-forgejo";
  rev = "v1.6.1";
  hash = "sha256-L32O7iCyqa1fO7ZCXJTYQiRSFSEkZ93PZz9wmBlSh3g=";
  vendorHash = "sha256-rMLIzfpqq1V4zujbUBU7KspBPkjJGGEIRxbc8SVP+/A=";
  spdx = "MPL-2.0";
  homepage = "https://registry.terraform.io/providers/svalabs/forgejo";
}
