# Forgejo's configuration, applied by the forgejo-tofu PostSync Job in the
# Forgejo Application (FORGEJO_MIGRATION_PLAN.md -> Secrets -> Forgejo
# configuration as OpenTofu). Providers are pinned by .terraform.lock.hcl.
# Credentials: the sealed Forgejo admin (FORGEJO_HOST, FORGEJO_USERNAME,
# FORGEJO_PASSWORD) -- tokens can only be created with basic auth.

terraform {
  required_providers {
    forgejo    = { source = "svalabs/forgejo", version = "1.6.1" }
    kubernetes = { source = "hashicorp/kubernetes", version = "3.3.0" }
    random     = { source = "hashicorp/random", version = "3.9.1" }
  }
  # State, and the tokens it mints, in a Secret in the forgejo namespace.
  backend "kubernetes" {
    secret_suffix     = "forgejo"
    namespace         = "forgejo"
    in_cluster_config = true
  }
}

provider "forgejo" {}

provider "kubernetes" {}

# --- Runner monitoring ---------------------------------------------------
# Forgejo's metrics have no runner state; json_exporter reads
# GET /admin/actions/runners with this bot's read:admin token
# (lab.jsonnet forgejo runner monitoring, ForgejoRunnerOffline).

resource "random_password" "monitor" {
  length  = 40
  special = false
}

resource "forgejo_user" "monitor" {
  login                = "monitor"
  email                = "monitor@forgejo.invalid"
  full_name            = "Runner monitor (OpenTofu)"
  password             = random_password.monitor.result
  admin                = true # /admin/actions/runners needs a site admin
  must_change_password = false
  visibility           = "private"
}

resource "forgejo_personal_access_token" "monitor" {
  user   = forgejo_user.monitor.login
  name   = "runner-monitor"
  # read:repository for the push mirrors' last_error (ForgejoPushMirrorFailing).
  scopes = ["read:admin", "read:repository"]
}

resource "kubernetes_secret_v1" "monitor_token" {
  metadata {
    name      = "forgejo-monitor-token"
    namespace = "forgejo"
  }
  data = {
    token = forgejo_personal_access_token.monitor.token
  }
}

# --- ftzm/dots ------------------------------------------------------------
# Migrated from GitHub with Forgejo's migrator (issues, PRs, numbers kept);
# taken over here, not created.

import {
  to = forgejo_repository.dots
  id = "ftzm/dots"
}

resource "forgejo_repository" "dots" {
  owner       = "ftzm"
  name        = "dots"
  description = "NixOS and k3s homelab configuration"
  private     = true
  has_wiki    = false
  # Its migration source, which the API reports; left out, the provider
  # plans it empty and fails ("inconsistent result after apply").
  clone_addr = "https://github.com/ftzm/dots.git"
  # Off until the workflows are ported to .forgejo/workflows: with that
  # directory absent Forgejo runs .github/workflows
  # (modules/actions/workflows.go ListWorkflows), the GitHub workflows,
  # without their secrets.
  has_actions = false
}
