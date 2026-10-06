# The scratch network's DNS allowlist (default.nix): the image registries the
# manifests pull from and their CDNs, and GitHub, ArgoCD's source until the
# nas mirror. Subdomains included. Everything else is refused.
[
  "github.com"
  "githubusercontent.com"
  "ghcr.io"
  "docker.io"
  "docker.com"
  "quay.io"
  "registry.k8s.io"
  "pkg.dev"
  "codeberg.org"
  "aws.com"
  "ecr.aws"
  "cloudfront.net"
  "amazonaws.com"
]
