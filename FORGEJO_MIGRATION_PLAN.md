# Forgejo Migration Plan

## Context

`ftzm/dots` lives on GitHub. It is public, fetched anonymously by comin on every
host but the pi (`role/comin.nix:9`; the pi's comin is disabled,
`machines/pi/default.nix:175`) and by ArgoCD
(`cluster/environments/lab/lab.jsonnet:400`). CI runs on GitHub-hosted runners
and throws away every closure it builds.

Goals, in priority order:

1. **Privacy** — the repo, its history, issues and CI logs stay in house. No
   third-party mirror or fallback.
2. **Own hardware for CI** — persistent nix store, real cores, aarch64 for pi.
3. **Keep the artifacts** — a CI build of a host closure is the closure that
   host deploys, not a throwaway.
4. **Workflows that touch the fleet** — post-merge convergence checks, per-host
   `nvd diff` on PRs, triage agents working from the CI log and release
   notes.

Current state:

- Forgejo `16.0.5` in-cluster on nuc (`cluster/manifests/forgejo/`),
  SQLite, `local-path` RWO PVC `forgejo-data` (20Gi, no prune guard), git SSH on
  NodePort 30022, HTTPS via traefik + LE wildcard. Nightly `forgejo dump` to
  NFS on nas → borgbase.
- Runner: NixOS microVM on nuc (`machines/nuc/forgejo-runner.nix`), SLIRP NAT
  outbound only, jobs run in containers via the guest podman socket
  (`docker://` labels), registered and working end to end.
- Five workflows in `.github/workflows/`: `ci.yml`, `update-flake-lock.yml`,
  `renovate.yaml`, `auto-fix-flake-update.yml`, `renovate-triage.yml`.

---

## Target State

| Concern | Today | Target |
|---|---|---|
| Source of truth | GitHub | Forgejo (in-cluster) for development; its `master` mirrored to nas, which ArgoCD and the writer deploy from |
| Host deploy | comin pulls `master` from GitHub, evaluates and builds on each host | **`fleet-agent`** on every host (pi included): reads the manifest pointer from nuc's `:5001/fleet/` over LAN (`192.168.1.4`: nas, pi) or Tailscale (`100.64.0.2`: laptops), realises the manifest (a store path signed by nuc's cache key), substitutes its own path, switches. No git, no evaluation, no comin |
| ArgoCD `repoURL` | GitHub https | bare mirror `ssh://git@192.168.1.3/dots.git` on nas, fed by a Forgejo push mirror; read-only key |
| CI | GitHub-hosted, ephemeral | Forgejo Actions: a host-mode runner on nuc registered to `dots` only (its CI and Renovate); the existing microVM registered to `ftzm/triage` only (the triage agent). No `dots` job runs on an instance-wide runner (Workflow security, rule 2) |
| Build artifacts | discarded | built pre-merge by **nuc's own nix daemon** (trusted jobs use it over the local socket), signed by nuc, served by harmonia on nuc; a writer on nuc publishes the manifest; hosts substitute |
| Deploy model | comin pull, evaluate + build per host | `fleet-agent` pull: manifest, substitute, switch — no evaluation anywhere but nuc |
| Renovate | `--platform=github`, PAT | `--platform=forgejo` from `dots`' own `renovate.yml`, Forgejo token of a `renovate` bot user |
| Triage agents | `anthropics/claude-code-action` | `claude -p` from the `claude-code-nix` input, in its own repo `ftzm/triage`, whose credentials cannot write `dots` |
| GitHub | repo + CI | flake inputs only |

**Pull, because laptops sleep and roam** — a push finds them only when they
happen to be awake. The writer runs seconds after a merge and the agent's
tick is two minutes. Push from a trusted service on nuc (deploy-rs) remains
the natural addition for the lab hosts if immediate push and connectivity
rollback are ever wanted. Laptops reach nuc over Tailscale at home and away
(the cluster already scrapes them by tailnet IP, `lab.jsonnet:47-52`; at home
the path is direct, `nuc.tail.ftzmlab.xyz → direct 192.168.1.4:41641`, per
`INCIDENT-2026-08-wireguard-transport.md`), so off-LAN they still fetch the
manifest and substitute — glue from harmonia through the home uplink, the
bulk from cache.nixos.org's CDN. Hosts set a short `connect-timeout` so a
dark home never stalls a nix command.

---

## Synergies

1. **CI build = the deploy.** `ci.yml` already builds the four x86 toplevels
   per PR. Built into nuc's store and served, the agent substitutes exactly
   those paths. Deploys become downloads and cannot fail mid-build.
2. **pi gets CI.** `.github/workflows/ci.yml:40` skips aarch64. `boot.binfmt.emulatedSystems
   = ["aarch64-linux"]` on nuc (the build host); its kernel and firmware come
   from `nixos-raspberrypi.cachix.org`, emulation covers the glue.
3. **Incremental flake bumps.** nuc's store persists across jobs, so a bump
   rebuilds only what changed. No `magic-nix-cache-action`.
4. **`nvd diff` per host as a PR comment.** Master's and the PR's closures are
   both in the cache; the diff needs no rebuild.
5. **Convergence check.** An alert on `fleet_deployed_commit` lagging the
   published manifest's commit per host, and on `fleet_last_failure`; no job
   needed.
6. **Triage without GitHub.** `claude -p` in the job, its OAuth token
   injected by a proxy on nuc, PR comments via the Forgejo API. The agent works from the CI log
   and the release notes — all its two jobs (fixing a failed Renovate build,
   reviewing a major) use; it stays isolated from the store and the other
   hosts (Design → Triage runner).

---

## Dependency Chain and Failure Scenarios

What a host traverses to fetch the repo from Forgejo as currently deployed,
using the https name:

```
comin ─DNS─▶ headscale split DNS (pi, machines/pi/default.nix:116-119)
             → resolver at 100.64.0.2 (nuc)
      ─net─▶ Tailscale to nuc
      ─TLS─▶ traefik pod (k3s on nuc) + LE wildcard (cert-manager pod)
      ─app─▶ forgejo pod → local-path PVC on nuc's disk
      ─mgmt▶ every pod above is deployed by ArgoCD, which fetches from forgejo
```

Every link but pi is on nuc, and nuc is a client of the repo. GitHub today
sits outside all of it. The forge has already broken its own deployer once:
a broken forgejo PreSync gate blocked every ArgoCD sync for 9 days (see
`LOGGING_PLAN.md`).

| # | Trigger | Breaks | Lockout | Recovery |
|---|---|---|---|---|
| S1 | nuc commit breaks k3s / traefik / networking / tailscale / harmonia (lab-pin bump, firewall edit) | Forgejo, harmonia, the writer — i.e. the whole deploy plane | no manifest is published and no closure is served, so nothing new deploys anywhere; nuc's own agent cannot fetch a fix that only nuc could publish | on every host touched by hand, `systemctl stop fleet-agent.timer` first; nuc: `nixos-rebuild --rollback` over ssh, or a workstation push; other hosts: workstation push if they need anything before nuc is back; then the fix to master and the timers restarted (see "nuc Down") |
| S2 | nuc commit wedges PID 1 / kernel panic; watchdog boot-loops the new generation | everything on nuc | as S1 | console on nuc (bootloader menu to the previous generation), then `systemctl stop fleet-agent.timer` on nuc before the manifest can switch it back; everything else waits or is pushed from the workstation, timer stopped first; then the fix to master and the timers restarted |
| S3 | Renovate bumps forgejo / traefik / cert-manager image; new version fails to start | Forgejo (and, for traefik/cert-manager, its https name) | none for ArgoCD: it reads the nas mirror. Forgejo's own repo is unavailable | a Forgejo upgrade that fails rolls itself back and locks further upgrades (Forgejo Upgrades); traefik / cert-manager: push the revert to the nas mirror (and to Forgejo once it is back, Decisions → nas mirror), ArgoCD deploys it |
| S4 | nuc disk dies, or `forgejo-data` PVC pruned | all Forgejo data | none for data (nas mirror and workstation clone for master, nightly dump; the manifest is regenerated from master) | a stale restored Forgejo cannot move deploys backwards: nas refuses its non-fast-forward mirror push (Decisions → nas `master`). Its own stale state still matters to the runner and Renovate — see Restore Runbook |
| S5 | pi (headscale) or nuc's resolver down; a host reboots meanwhile | name resolution of `*.lan.ftzmlab.xyz` | CI, Renovate and the VM runner reach Forgejo by its https name (Renovate's `--endpoint`, the checkout's `$GITHUB_SERVER_URL` = `ROOT_URL`, `lab.jsonnet:3137`, `forgejo-runner.nix:20`), which needs DNS, traefik and cert-manager: they stop until it resolves | deployers are unaffected — they use IPs (nas/pi fetch manifest and cache from `192.168.1.4`, ArgoCD and the writer the nas mirror at `192.168.1.3`, laptops `100.64.0.2`). A fix needed meanwhile goes to the nas mirror (Decisions), no CI needed. Laptops still depend on headscale on the pi as the tailnet control plane; accepted |
| S6 | Renovate bumps the runner image to a broken one | CI | fix PR can't go green; auto-merge blocked | merge by hand; not a lockout |
| S7 | away from home | laptop's fetches go over Tailscale through the home uplink | none — deploys still work, slower (bulk from the CDN); with home dark the laptop stays on its current config and can rebuild itself locally | `connect-timeout` on hosts, `--max-time` in the agent |

Two distinct problems:

1. **Fetch-side circularity** (S1, S2, S5, S7): hosts fetch closures and the
   manifest from nuc, which they also deploy. Only nuc can publish, so a
   second git copy does not help here; the manual paths do (nuc Down).
2. **Deploy-of-the-forge + restore hazard** (S3, S4): ArgoCD would fetch its
   source from the Forgejo it deploys. Broken by the nas mirror (Decisions):
   ArgoCD and the writer read a git copy that depends on nothing in the
   cluster.

---

## Decisions

**nas `master` is the deploy source of truth.** Neither deployer — ArgoCD
or the fleet writer — fetches from Forgejo: a broken or lost Forgejo would
then block the very sync that repairs it, and a rebuilt cluster could not
bootstrap. Keeping the GitOps source outside what it deploys is the standard
fix for this known antipattern. The source is a git repo, not Forgejo:

- Forgejo push-mirrors `master` of `dots` to a bare repo on nas,
  `ssh://git@192.168.1.3/dots.git`, with `sync_on_commit: true` and
  `branch_filter: master` (`modules/structs/mirror.go:14-16`; without a
  filter it pushes every branch with `+refs/heads/*`,
  `services/mirror/mirror_push.go:78`), declared by the Forgejo OpenTofu job (Secrets).
- The nas repo sets git's `receive.denyNonFastForwards = true`, so nas
  `master` only ever moves forward. Whichever of Forgejo and nas is ahead
  wins: a stale Forgejo (a restored dump, a force-push) has its mirror push
  refused, and the refusal shows in the push mirror's `last_error`. An alert
  reads it from `GET /repos/ftzm/dots/push_mirrors`.
- nas serves it with its own sshd from its NixOS config: a `git` user with
  `git-shell` and three authorized keys — Forgejo's push-mirror key (write),
  and ArgoCD's and the writer's keys restricted to `git-upload-pack` (read).
  No dependency on k3s, traefik, cert-manager or nuc.
- ArgoCD's `repoURL` is the nas mirror, with nas's committed host key
  (`secrets/secrets.nix`) in `configs.ssh.extraHosts`; the writer polls it
  (Binary Cache → Design → Writer).
- **During a Forgejo outage, a fix goes to nas** (every workstation clone
  has it as a second remote): ArgoCD and the writer deploy it. Once Forgejo
  is back its mirror push is refused until the same commit is pushed to
  Forgejo — the `last_error` alert says so.

**nuc is the deploy plane.** Forgejo, the runners, nuc's store, harmonia and
the fleet writer all live on nuc. When nuc is down, nothing deploys anywhere,
and that is accepted as the price of one build store — it is the same
dependency the whole cluster already has. Two manual paths replace
automation in that window (see "nuc Down"): a workstation push to any host,
and a laptop rebuilding itself.

**No comin.** Hosts run `fleet-agent` (Binary Cache → Design); comin leaves
every host in step 2.

**Transport: LAN for lab hosts, Tailscale for laptops; nothing dials wg.**
This follows `INCIDENT-2026-08-wireguard-transport.md` §Design, which retired
wg "as anything a service *dials*" after the nas↔nuc tunnel sat dead for 32
days unnoticed; `role/lab.nix` already sends Loki traffic to
`machines.nuc.lan`. nas and the pi fetch from `192.168.1.4`, laptops from
`100.64.0.2`, nuc reads locally. nuc's `wg0` stays only as the bind address
traefik and blocky already use. The pi does not join wg. The manifest
pointer is unsigned plain HTTP: the LAN is trusted, and this plan adds no
per-asset defences against an attacker on it.

**Forgejo stays in-cluster.** With ArgoCD reading the nas mirror, Forgejo
is an ordinary ArgoCD-deployed app: image bumps stay on the Renovate→ArgoCD
path, and a broken Forgejo does not block its own repair. Upgrades stay
unattended; a failed one rolls itself back (Forgejo Upgrades).

**ArgoCD guards.** `argocd.argoproj.io/sync-options: Prune=false` on
`forgejo-data` and `forgejo-backup`. Restores follow the Restore Runbook:
automated sync on the Forgejo Application is paused
(`spec.syncPolicy.automated.enabled: false`, ArgoCD's documented switch), the dump is restored with Actions
disabled and the ssh/https services down, refs are brought current from a
live clone before Forgejo starts, then Forgejo is exposed and sync resumes.
The dump supplies issues/PRs/settings; repo content always comes from a live
clone.

**Manual paths test themselves.** The workstation → host push and the
laptop self-rebuild run weekly in their non-switching forms and report a
metric; an alert fires on failure or staleness (nuc Down → Self-tests), so a
stale clone or a missing key surfaces then, not during an outage.

### Workflow security

Threat: a stranger steering a job that holds privilege. Today that is text
by anyone but a dependency's own maintainers — an issue, a discussion, a
web page — reaching `claude -p`. Once the instance hosts other repos, or
opens registration, it is also a stranger's PR and a stranger's job on a
shared runner. The design must stay secure if the instance goes public,
and if `dots` itself were ever made readable: it rests on neither.

**Trust in upstream.** The maintainers of every dependency — flake inputs
(direct and transitive), container images, Helm charts — are trusted
outright, fleet-wide, including with reading `dots`. Their code already
runs unread on every machine: NixOS and home-manager modules get `inputs`,
`inputs.self` included (`flake.nix:123,152,157,173`), run as root, and a
hashed `builtins.fetchurl` in evaluation or a fixed-output derivation on
nuc reaches the network (T17). Trusting them to run on the hosts but not to
run builds on nuc would be incoherent, so nothing here defends against
them. What is defended is everyone else: strangers' PRs and jobs, and text
from anyone but a dependency's maintainers reaching the triage agent.

Forgejo `v16.0.5` / forgejo-runner `v13.2.0` behaviour it rests on:

- **The job is the smallest unit of privilege.** A non-fork job's automatic
  token has write on every unit of its repo; a fork PR's is read-only
  (`models/perm/access/repo_permission.go:146-151`). The workflow
  `permissions:` key is ignored ("not supported in Forgejo and will be
  ignored", `options/locale_next/locale_en-US.json:913`). Every non-fork job,
  and every `pull_request_target` job including fork ones, receives every
  repo and owner secret (`services/actions/secret.go:41,53`). The runner puts
  the automatic token into every step's environment as `GITHUB_TOKEN` and
  `FORGEJO_TOKEN` (runner `act/runner/run_context.go:1707-1711,1731`), and
  the runner process holds everything of every job it runs. Nothing inside a
  job is separated from anything else in it.
- A pushed branch runs *its own* workflow files without approval; only
  pushes by the Actions user are skipped
  (`services/actions/notifier_helper.go:137`, `services/actions/trust.go:81-82`).
  A PR merge through the API is a push whose pusher is the merging user
  (`services/pull/merge.go:384`).
- A fork PR's own workflow files wait for approval and get no secrets. AGit
  PRs (`git push origin HEAD:refs/for/<target-branch>/<topic>`) are fork PRs
  (`models/issues/pull.go:479-482`) although their head repo is the base
  repo (`services/agit/agit.go:144`); anyone with read can open one
  (`routers/private/hook_pre_receive.go:112-117`), and `refs/for/…` fires no
  `push` workflows (`routers/private/hook_post_receive.go:60,96`).
- `pull_request_target` runs the base branch's workflow for every PR, forks
  included, and never waits for approval (`notifier_helper.go:258-274`; the
  approval flag is set only on the head's workflows, lines 284-296).
- The required status check is not a security gate: any user with code
  write can post any context (`routers/api/v1/api.go:1017`), contexts are
  matched by string (`services/pull/commit_status.go:139`,
  `services/actions/commit_status.go:113`), and a new status starts the
  auto-merge check (`services/repository/commitstatus/commitstatus.go:120`).

Since privilege cannot be narrowed inside a job, the boundaries are the
repo, the runner's registration scope, and the identity a job acts for.

**Rules:**

1. **Privileged jobs gate on who the PR or comment is from — never on
   branch names or the head repo.** Every `dots` job triggered by a PR or a
   comment compares the author (`github.event.pull_request.user.login`, or
   the comment's `user.login`) with a fixed list, and stops at its first
   step otherwise — `build` by failing, so the required status never passes
   and the PR cannot merge. A fork branch can be named `renovate/x`, and an
   AGit PR's head repo is `dots` itself, so neither says who wrote the PR.
   The lists are in the ported set.
2. **`dots` jobs run only on runners registered to `dots`; triage jobs only
   on the runner registered to `ftzm/triage`.** A repo-scoped runner is
   invisible to every other repo, whatever labels that repo's workflows ask
   for. No `dots` or triage job targets an instance- or user-level runner,
   so `dots`' secrets never sit in a runner process that another repo's jobs
   reach.
3. **No user- or instance-level secrets or variables.** A user-level secret
   reaches every repo of that user (`models/secret/secret.go:186`), including
   any public one strangers can PR. Every secret is a repo secret.
4. **Nothing depends on repo or instance visibility.** Every check above
   holds with `dots` public and registration open: a stranger's PR fails
   `build` at the author check, before any evaluation or build. Privacy (goal 1) is a separate property; security does not
   rest on it.

**Where untrusted content runs.** Only where it holds nothing:

- **PR code** — PRs by the author list only (rule 1): pure nix evaluation
  of the PR's flake in the job, every build and check in nuc's nix
  sandbox. Pure mode restricts the evaluator's builtins, but resolving
  inputs comes before it, and an input missing from `flake.lock` is
  fetched to lock it — a `path:<absolute path>` input reads the host path
  (T15), here the runner's saved login. Every evaluation therefore runs
  with `--no-update-lock-file`: nix then refuses any lock change, and a
  `path:` entry without `narHash` is refused as unlocked (T15); a wrong
  hand-written `narHash` makes the error print the file's real hash, a
  sha256, never its content. A lock entry that does read a runner file can
  only come from Renovate's own lock step, i.e. from a flake input's
  maintainer, who is trusted (Trust in upstream). The flag is also a correctness requirement: an
  input missing from the committed lock would otherwise be resolved at
  evaluation time, so CI and the writer could each resolve a `github:`
  input to a different revision and CI would prove a closure other than
  the one published (Binary Cache, goal 2). `ci.yml` is `pull_request_target`, so a PR cannot change what CI
  does and no run waits for a human.
- **Upstream-authored text** — read only by the triage agent, which runs in
  its own repo `ftzm/triage` on that repo's runner. Its automatic token can
  write only `ftzm/triage`; its one secret is the `triage` bot's token,
  which reads `dots`, opens AGit PRs and comments there, and has no access
  to `ftzm/triage` — so a compromised VM cannot change what later triage
  jobs run (dispatch needs Actions write, `routers/api/v1/api.go:904`,
  which in a user repo is write on every unit including code,
  `models/perm/access/repo_permission.go:254-266`, and a pushed branch runs
  its own workflows); the dispatch comes from `dots` with a separate
  `triage-dispatch` identity (Secrets); the Claude token never
  enters it (Containing the agent). Its
  results reach `dots` only as input that trusted `dots` jobs check: an AGit
  PR (a fork PR, rule 1 gates what acts on it) or a comment carrying a
  verdict. `merge-repair` refuses any repair outside Rule R (Autonomy),
  whose allowlist never admits `.forgejo/`: once merged into `renovate/*` a
  workflow change would be pushed as `merger` and run with every `dots`
  secret. The check lives in the job, not in
  branch protection: a `protected_file_patterns` rule on `renovate/*`
  refuses protected-file changes on every direct push regardless of
  whitelist (`routers/private/hook_pre_receive.go:364-385`), and Renovate's
  `github-actions` manager (enabled, `cluster/renovate.jsonnet:64`) updates
  `.forgejo/workflows` (renovate `lib/modules/manager/github-actions/index.ts:17`).

What can therefore reach master: the owner; Renovate's automerge of its own
PRs once `build` passes; through a Renovate branch, a triage repair that
passes `build` and stays within Rule R (Autonomy); and a `merge` verdict on a
CI-passing, non-stateful Renovate PR that did not automerge — the agent
decides *whether* Renovate's tested content merges, never *what* merges.

**Containing the agent.** The triage agent processes untrusted input and
reads private data (`dots`), so by Meta's [Agents Rule of
Two](https://ai.meta.com/blog/practical-ai-agent-security/) — an agent may
combine at most two of untrusted input, sensitive data, and changing state
or communicating externally — its communication is what gets cut. The
design is the standard one: the read-only agent job with separate write
jobs ("safe outputs") and an allowlisting egress proxy of [GitHub Agentic
Workflows](https://github.github.com/gh-aw/introduction/architecture/), and
the credential-injecting proxy of Anthropic's [Securely deploying AI
agents](https://code.claude.com/docs/en/agent-sdk/secure-deployment):

- **Safe outputs.** The agent writes files only: a patch, its notes, a
  verdict. Trusted steps publish them — the AGit push, the comment — and
  render the agent's text inert (inside a fence longer than any backtick run
  in it, so an embedded `![](https://…)` never loads in a reader's browser);
  the AGit PR's title and description are the trusted step's, not the
  agent's. What they may do in `dots` is gated by rule 1 there.
- **No egress, no reads outside the workdir.** Every shell command the
  agent runs goes through Claude Code's sandbox (sandbox-runtime:
  bubblewrap, no network namespace, traffic only through its allowlisting
  proxy), set in the VM's managed settings: `sandbox.enabled`,
  `sandbox.failIfUnavailable: true`, `sandbox.allowUnsandboxedCommands:
  false`, `sandbox.network.allowedDomains = []`, and
  `sandbox.filesystem.denyRead = ["/"]` with `allowRead` only the job
  workdir (the agent's store included) and the paths its tools run from.
  Claude Code unions these arrays across every settings scope — managed,
  CLI, the project's `.claude/settings.json` and `.claude/settings.local.json`,
  user — so the managed-only switches
  `sandbox.filesystem.allowManagedReadPathsOnly: true` and
  `sandbox.network.allowManagedDomainsOnly: true` make the managed
  `allowRead` and `allowedDomains` the only ones counted. Without them the
  checkout's own tracked `.claude/settings.local.json` (the owner's
  workstation policy: `WebSearch`, `Read(//tmp/**)`, …) merges into the
  agent's, and any project settings file widens reads and egress; with
  them a project file setting `enabled: false`, a wider `allowRead` and an
  allowed domain changes nothing (tested).
  The read rule is needed because the sandbox's default is "read access to
  the entire computer, except certain denied directories"
  (code.claude.com/docs/en/sandboxing), and the agent runs as the same uid
  as the host-mode runner daemon, whose saved login `.runner` sits in
  `/var/lib/gitea-runner/<name>` (nixpkgs `gitea-actions-runner.nix:219,231`);
  whoever holds that login polls as the triage runner and receives later
  triage jobs with `TRIAGE_TOKEN`. Everything the agent needs, the tools
  its commands run included, is put into its store before it runs by the
  trusted gather step (Autonomy, rule L), so it needs no network.
- **Bash is the agent's only tool.** The sandbox wraps only the Bash tool's
  child processes; Claude Code's own `Read`, `Edit`, `Write`, `Grep`,
  `Glob`, `NotebookEdit`, `WebFetch` and `WebSearch` execute in the
  unsandboxed Claude Code process as the runner uid, governed only by
  permission rules (tested: `Read` returned a `.runner` the sandbox masked
  for Bash; `Write`/`Edit` could overwrite the runner's state or the
  scripts the trusted publish steps run next; `WebSearch` sends a query
  out, and web pages have arbitrary authors). The VM's managed settings set
  `permissions.deny` for all eight and `allowManagedPermissionRulesOnly:
  true`, so no CLI flag or project file re-allows one (tested: the CLI's
  `--allowedTools` is ignored, `Write` is "disabled for this session"). The
  agent reads and edits through shell commands inside the sandbox. Path-scoped
  rules cannot replace this: the agent's cwd is itself under
  `/var/lib/gitea-runner/<name>/.cache/act` (forgejo-runner
  `internal/pkg/config/config.go:534`), and a deny beats any allow. The agent's
  `nix` uses a store of its own in the workdir, with no daemon socket in
  the sandbox: the VM's nix-daemon runs outside it, and a fixed-output
  build would fetch any URL.
- **What the agent can write, and what nothing trusted runs from.**
  Sandboxed Bash writes only to its cwd and to Claude Code's `TMPDIR`,
  `/tmp/claude-<uid>` (tested: `$HOME` and `/tmp` refused, "Read-only file
  system"). forgejo-runner gives each job
  `<workdir_parent>/<random8>/{hostexecutor,act,tmp}` — steps run in
  `hostexecutor/` — and deletes it at job end
  (`act/runner/run_context.go:330-373`,
  `act/container/host_environment.go:302-310`); `/tmp/claude-<uid>` lies
  outside it and survives from one triage job to the next. So the gather
  step builds `hostexecutor/agent/` holding only the agent's inputs (`dots`
  at the tested sha, the distilled log, the release notes, its store) and
  an `out/` directory, and `claude` runs with `agent/` as its cwd; the
  trusted steps run only scripts from the `ftzm/triage` checkout outside
  `agent/` and read `agent/out/` as data. The gather step empties
  `/tmp/claude-<uid>` before `claude` starts and an `if: always()` last
  step empties it again, so nothing one run's agent leaves reaches the
  next run's (Autonomy, rule L).
- **Credentials outside the boundary.** The Claude token never enters the
  VM: the agent's `ANTHROPIC_BASE_URL` points at a proxy on nuc that adds
  the token and forwards only the Messages API (Plumbing). The one secret
  in the VM is the `triage` bot token, used only by the trusted gather and
  publish steps.

With an empty allowlist the agent's only outbound channel is its own
Messages API traffic through the proxy, to Anthropic. The allowlist stays
empty for Rule L: any reachable host is a source of text by authors other
than the bumped dependency's maintainers (and, per the Anthropic guide's
domain fronting through CDN-fronted hosts, of any host behind the same
CDN).

**Autonomy.** A repair is written from untrusted input, with private data,
and changes what hosts run — all three Rule of Two properties. It stays
autonomous, with no human approval, by removing the untrusted-input leg the
way Meta's post does ("author lineage filtering") and the design-patterns
paper's action-sandboxing does (the agent restricted "to only trusted
documentation and code sources", [Design Patterns for Securing LLM Agents
against Prompt Injections](https://arxiv.org/abs/2506.08837), software
engineering case study). The basis: a dependency's maintainers are trusted
outright (Trust in upstream), so an injection that comes only from them
gives them nothing they lack. Two mechanical rules:

- **Rule L — lineage: the agent reads only text by trusted authors.** The
  trusted gather step assembles
  everything: `dots` at the tested sha; the distilled CI log (output of nix,
  nixpkgs, and packages already in the closure); the bumped dependency's
  release notes and changelog from its own repository at its release tags
  (the GitHub releases API and the files at both tags — never issues,
  discussions, or other repos); its flake inputs via `nix flake archive`
  (root and `cluster/`) into the agent's store; the closures of every tool
  the agent's commands run (the cluster dev shell among them) into the same
  store; and the bumped chart via `tk tool charts vendor`. The agent has no
  web access and its shell no network (Containing the agent), so it cannot
  pull in text from anyone else.
- **Rule R — a repair may change only what `build` proves.** `build`
  evaluates and builds; it says nothing about repo files that run outside
  the nix sandbox, with the privilege of whoever runs them:
  `cluster/flake.nix`'s dev shell (the Renovate job runs
  `nix develop ./cluster`, `renovate.yaml:50-51`), `cluster/Justfile`
  (Renovate's post-upgrade `just render-lab`,
  `cluster/renovate.jsonnet:106-108`), `flake.nix` (`nixConfig`, taken
  with `--accept-flake-config` by the `Makefile`'s `sudo nixos-rebuild`),
  the `Makefile`, `dotfiles/` (stowed into `$HOME`; `~/.config/emacs/init.el`
  links into the checkout), `.claude/` and `CLAUDE.md` (the owner's Claude
  Code sessions), `bin/` (on `PATH` and sway keybindings), `.forgejo/`.
  A mistaken edit there would pass `build` and then run untested.
  So repairable paths are an **allowlist** of paths whose only consumers
  are the nix sandbox and pure evaluation: `role/`, `machines/`,
  `cluster/environments/`, `cluster/lib/`, `cluster/charts/`,
  `cluster/manifests/`. Every file there with a consumer enters a build
  through a nix path (`builtins.readFile`, `${./file}`, `patches`, uv2nix
  `workspaceRoot`), nothing there turns a repo path
  into a live string path, and every live reference to the checkout
  (`$HOME/dots/bin`, `stow … dotfiles`, `~/dots/stacks.jpg`) points outside
  it. `flake.lock` is outside it too, so a repair keeps the Renovate head's
  inputs. A repair touching any other path — including a new top-level one —
  is refused and the Renovate PR labelled `needs-review`. No bound on which
  hosts or namespaces a repair changes: with the maintainers trusted
  (Autonomy) and the path allowlist confining it to what `build` proves,
  such a bound protects nothing.
  The cluster paths stay repairable only because nothing
  renders them outside the sandbox: Renovate's post-upgrade render is a
  sandboxed build (`renovate.yml`), since `tk` outside it lets jsonnet read
  any host file (`importstr "/etc/hostname"` returned it) while inside the
  sandbox the same import fails.

The same basis covers review verdicts: an injected `merge` in X's release
notes gets X's maintainers nothing they lack;
`triage-verdict`'s stateful list stays.

---

## Hurdles Inventory (Forgejo 16.0.5 docs and source)

| Item | Where | Finding | Replacement |
|---|---|---|---|
| `workflow_run` trigger | `auto-fix-flake-update.yml:44`, `renovate-triage.yml:43` | **Unsupported.** Absent from the event switch in `modules/actions/workflows.go` and from the docs' event list | a `dispatch-triage` job in `ci.yml` (`needs: build`, PRs authored by `renovate` only — Renovate also owns `flake.lock`, so no other PR needs triage) chooses the mode from `needs.build.result` and dispatches `ftzm/triage`'s workflow (the ported set). `needs`, `always()`, `github.event.pull_request.number`, `github.run_id`, `github.server_url` all exist |
| Workflow directory | `.github/workflows/` | Forgejo uses the **first existing** of `.forgejo/workflows`, `.gitea/workflows`, `.github/workflows` — "no matter whether it contains workflows or not" (`ListWorkflows`); it never merges them. So Forgejo runs the GitHub workflows only while `.forgejo/workflows/` is absent | create `.forgejo/workflows/` first (even with one file) and Forgejo ignores `.github/` from then on; no gating needed |
| `uses:` resolution | all | bare `actions/checkout@v7` resolves against `DEFAULT_ACTIONS_URL` = `https://data.forgejo.org` (instance has no override; `[actions]` is just `ENABLED = true`). Fully qualified URLs to any Forgejo or GitHub instance are supported and "strongly recommended" | the nix jobs use **no third-party actions**: checkout is `git clone "$GITHUB_SERVER_URL/$GITHUB_REPOSITORY"` with the automatic token. JS actions in host mode would need node on the runner; avoided entirely |
| `gh` CLI | both triage workflows, `update-flake-lock.yml` | GitHub-only. The Forgejo API has every call needed (checked in the live swagger): PR list/get/files/commits, `POST pulls/{i}/merge` with `merge_when_checks_succeed`, `issues/{i}/comments`, `commits/{ref}/status`, `actions/runs`, `actions/runs/{id}/jobs`, `actions/jobs/{id}/logs`, `actions/workflows/{file}/dispatches` | `bin/forgejo-api` — a curl+jq wrapper; workflow logic moves into `bin/` scripts testable from the workstation |
| `anthropics/claude-code-action@v1` | `auto-fix-flake-update.yml:238`, `renovate-triage.yml:249,339` | GitHub-only by construction (GitHub API + OIDC) | `claude -p` from the `claude-code-nix` input in `ftzm/triage`, OAuth token injected by the proxy on nuc (Workflow security, Containing the agent), prompts and agent scripts in that repo, PR comments via the API as the `triage` bot |
| `actions/upload-artifact@v7` | triage transcripts | Forgejo artifacts need **v3 or the patched fork** `https://code.forgejo.org/forgejo/upload-artifact@v4`; 90-day expiry; no cross-run access | the patched fork, or write transcripts to the nas NFS share |
| `DeterminateSystems/*` (`determinate-nix`, `nix-installer`, `magic-nix-cache`, `update-flake-lock`) | `ci.yml`, `renovate.yaml`, triage, `update-flake-lock.yml` | GitHub-hosted-runner conveniences | none needed on a NixOS host-mode runner with nuc's daemon behind it; the flake-lock bump becomes Renovate's `nix` manager with `lockFileMaintenance` — no separate job |
| `dorny/paths-filter@v4` | `ci.yml:16` | JS action | `git diff --name-only $base...$head` |
| `runs-on: ubuntu-latest` | all | runner `nuc-microvm` (v13.2.0) currently advertises `docker`, `ubuntu-latest` | host-mode label (`nixos:host`) for the nix jobs |
| `concurrency` | `renovate.yaml` | supported since v14; **default `cancel-in-progress: true` for push/PR** | `renovate.yml` sets `cancel-in-progress: false` explicitly; `ci.yml` keeps the default (a superseded PR build is cancelled) |
| Renovate | `renovate.yaml` | `--platform=forgejo` exists; platform automerge supported on ≥ v10 | `--platform=forgejo --endpoint https://forgejo.lan.ftzmlab.xyz/api/v1`, `RENOVATE_TOKEN` = Forgejo PAT (repo rw, user r, issue rw), a `dots` repo secret |
| Secrets `PAT_TOKEN`, `RENOVATE_TOKEN`, `CLAUDE_CODE_OAUTH_TOKEN` | all | `GITHUB_TOKEN`/`FORGEJO_TOKEN` is automatic. **Checked 2026-10-06** (a job on a private repo whose `main` had push and merge whitelists naming only the owner): `git push` to `main` is refused, to an unprotected branch allowed, and every pull-request API call (`POST /pulls`, so merge and `/pulls/{i}/update` too) returns 404 "Can't read pulls or can't read UnitTypeCode" — git access uses the task's own write permission (`GetActionRepoPermission`, `models/perm/access/repo_permission.go:143-165`), but the PR routes check the doer (`routers/api/v1/repo/pull.go:1166-1181`), the Actions bot user, which has no access to a private user-owned repo. So no PR operation on `dots` can use the automatic token. `PUT /repos/{o}/{r}/actions/secrets/{name}` exists | no GitHub PAT at all, and the Claude token is no Actions secret (it stays with the proxy on nuc); the Forgejo OpenTofu job (Secrets) writes the repo secrets, so they are reproducible from the repo; the `triage` bot token — read on `dots`, no access to `ftzm/triage` — is a secret of `ftzm/triage` only; the `triage-dispatch` bot token — write on `ftzm/triage`, no access to `dots` — is a `dots` secret used only to dispatch; the `merge-repair` and `triage-verdict` jobs use a `merger` token (Workflow security) |
| ArgoCD ssh known hosts | `argocd-ssh-known-hosts-cm` (GitHub keys only) | chart-generated (`helm.sh/chart: argo-cd-10.9.2`) | nas's committed host public key (`secrets/secrets.nix`) through the chart value `configs.ssh.extraHosts` (`charts/argo-cd/values.yaml:530`) |
| Branch protection | none on Forgejo yet | `POST branch_protections` supports `enable_status_check` + `status_check_contexts`, `block_on_outdated_branch`, `push_whitelist_deploy_keys` | `master`: required status = the build job, **`block_on_outdated_branch = true`** (CI must have run on the exact tree that merges — otherwise the writer's post-merge build silently becomes the real test). Every merge therefore makes every other open PR outdated. Auto-merge-armed PRs are brought current by `update-prs.yml` (the ported set) — including triage-repaired Renovate PRs, which Renovate itself stops updating once another user has committed to them ("If you push a new commit to a Renovate branch … Renovate stops all updates of that branch", Renovate docs); PRs without auto-merge (majors) are rebased by Renovate, `rebaseWhen: behind-base-branch` in `cluster/renovate.jsonnet` (the default `auto` rebases only `automerge: true` PRs). Cost, measured on 90 days of master: bot merges are 1–8 a day with one real wave, 12 Renovate/Dependabot PRs in twelve minutes on 2026-07-18. Those PRs touch `images.libsonnet`/`chartfile.yaml` — k8s manifests, **not** host closures — so each rebase re-runs a cached `nix build .#fleet` plus `render-lab`/`test-rules`: seconds to a minute each, ~10–15 min of nuc for a 12-PR wave. The expensive run (a nixpkgs bump changing all five closures) is one PR every three days, never a wave |
| Runner shape | `machines/nuc/forgejo-runner.nix` | — | two runners, each registered to one repo (Workflow security, rule 2): nixpkgs' `services.gitea-actions-runner` in host mode on nuc, registered to `dots` (`nixos:host`), for CI and Renovate; and the existing VM, re-registered from instance scope to `ftzm/triage` and switched to the same module in host mode inside the guest (label `triage:host`), for the triage agent. Other repos' CI gets its own runner when needed (Later); see Binary Cache → Design |

### The ported workflow set

Five GitHub workflows become four in `dots` and one in `ftzm/triage`, plus
the writer service on nuc, with the logic in scripts. Every `dots` workflow
runs on the `dots` runner; the triage workflow on the `ftzm/triage` runner
(Workflow security, rule 2).

- **`ci.yml`** (`pull_request_target` — the workflow always comes from master, so a PR cannot change what CI does and no run waits for approval; Workflow security):
  - Job `build` — first step: the author check (Workflow security, rule 1) — PR author the owner, `renovate` or `triage`, else fail. Then clone the PR head, evaluate `git+file://<clone>?rev=<sha>` with `--no-update-lock-file` in the job — the reference the writer uses, so CI builds exactly what the writer publishes — and build the resulting `.drv` paths — **every host, every PR, no path filter** (unchanged hosts are cache hits that cost seconds and yield the same path; a filter would let closure-changing non-`.nix` files — `iosevka.toml` via `builtins.readFile`, dotfiles copied into the store — merge unbuilt, to be built for the first time post-merge on nuc), and every other check as a **flake check built by nuc's daemon in the sandbox** — no network, no host files, no secrets: `checks.x86_64-linux.render-lab` (defined in `cluster/flake.nix`, so it uses the same tanka/jsonnet/helm as `just render-lab`; the root flake tracks different nixpkgs, and a version skew would change the output; CI builds the cluster flake's checks alongside the root ones; runs `tk export` and **fails if the output differs from the committed `cluster/manifests`**, which is what ArgoCD deploys (one directory per Application, `ARGOCD_APPLICATIONS_PLAN.md` step 2); the GitHub `ci.yml` re-renders but never compares), `checks.x86_64-linux.test-rules` (also in `cluster/flake.nix`), `checks.x86_64-linux.mailsort` (root flake). `just render-lab` and `just test-rules` on a copy of `cluster/` succeed with networking removed (`unshare -rn`), output byte-identical to the committed manifests. Outside the nix sandbox the PR's flake is only evaluated, purely and with `--no-update-lock-file` (Workflow security, "Where untrusted content runs"). Only the `nvd` comment step maps `MERGER_TOKEN` into its `env:`. `nvd diff` per host posted as a PR comment (the current path from the published manifest); on AGit PRs the automatic token is read-only (`models/perm/access/repo_permission.go:149-151`), so the comment posts with the `merger` token.
  - Job `dispatch-triage` — `needs: build`, `if: always()`, PRs authored by `renovate` only (rule 1). Mode: **fix** if `build` failed; **review** if it passed but the PR will not automerge (auto-merge not armed, or any `| major |` row in its body — today's test, `renovate-triage.yml:100-109`); otherwise nothing. At most one repair per Renovate PR: fix mode is skipped, and the PR labelled `needs-review`, when its branch already carries a `merge-repair` merge. Looks up the failed job's id (`GET /repos/ftzm/dots/actions/runs/{run_id}/jobs`) and dispatches `POST /repos/ftzm/triage/actions/workflows/triage.yml/dispatches` with `pr`, `mode`, `sha` (the head `build` tested) and `job_id`, using `TRIAGE_DISPATCH_TOKEN` in that step's `env:` only.
  - Job `merge-repair` — `needs: build`, only when the PR's author is `triage` (rule 1) and its base starts with `renovate/`. Runs `bin/repair-paths` from master's checkout — every trusted job runs `bin/` from master and treats the PR head as data — on `git diff --name-only $base...$tested_sha`; refuses a repair that touches a path outside Rule R's allowlist (Workflow security, Autonomy) and labels the Renovate PR `needs-review`. Otherwise `POST /repos/ftzm/dots/pulls/{i}/merge` with `head_commit_id` = `$tested_sha` (Forgejo refuses the merge if the head moved, `services/pull/merge_prepare.go:59`), so the check sees exactly the tree that merges. The Renovate PR then goes green and Renovate automerges it into master. `merger` has write on `dots` and is in master's merge whitelist (for `triage-verdict`), not its push whitelist. Master merges stay subject to the required `build` status.
  - `MERGER_TOKEN` is referenced only in the `env:` of the `merge-repair` merge step and the `nvd` comment step — no job-level `env:`, so no other step's process sees it.
- **`triage-verdict.yml`** (`issue_comment`, which always runs the default branch's workflow, `modules/actions/github.go:40-47`) — acts on a review verdict. Only for a comment by `triage` on a PR authored by `renovate` (rule 1). Parses the comment's verdict line (`triage-verdict: merge <sha>` or `triage-verdict: needs-review`). Merges with `head_commit_id` = that sha only if: the verdict is exactly `merge`; no dependency in the PR is on the stateful list `bin/triage-stateful` (Postgres/CloudNativePG, vectorchord, valkey, forgejo, immich, audiobookshelf, navidrome — kept in the trusted job, not the prompt). Otherwise it adds the `needs-review` label. Uses `MERGER_TOKEN` in the merge step's `env:`.
- **`update-prs.yml`** (`push` to `master`) — keeps auto-merge-armed PRs current with master, the pattern of [tibdex/auto-update](https://github.com/tibdex/auto-update) ("the missing piece to really automatically merge pull requests when strict status checks are set up"), ported because that action calls GitHub's endpoint. `bin/update-prs`: list open PRs whose head is a `dots` branch (not AGit) with auto-merge armed; for each behind master, `POST /repos/ftzm/dots/pulls/{i}/update` ("Merge PR's baseBranch into headBranch", `routers/api/v1/repo/pull.go:1240`). CI reruns on the updated head and the armed auto-merge completes. Token: `MERGER_TOKEN` in this step's `env:` only — the automatic token cannot call PR endpoints on a private repo (hurdles table, checked 2026-10-06). The merge into a `renovate/*` branch fires that branch's workflows as a push by the token's user; that branch's content is Renovate's plus triage repairs already refused if they touch `.forgejo/` (`merge-repair`).
- **`renovate.yml`** — in `dots`, `RENOVATE_TOKEN` a `dots` repo secret; `runs-on: nixos:host`; targets `ftzm/dots`. Schedule (daily, as today) + dispatch only: the push-paths trigger existed to regenerate PRs conflicted by `images.libsonnet`, which the per-image split (step 1) removes, and outdated PRs are brought current by `update-prs.yml`. Otherwise as today with the Forgejo platform flags; `concurrency` explicit. **It owns `flake.lock`**: in `cluster/renovate.jsonnet` (the source; `renovate.json` is generated from it by `just generate-renovate`, `cluster/Justfile:54-56`, and every Renovate change in this plan goes there), `enabledManagers += ["nix"]`, `lockFileMaintenance = { enabled = true; schedule = ["every 3 days"]; automerge = true }` (its own `automerge`: the general rule matches only `minor`/`patch`/`digest`, `renovate.json:199-207`, and lockfile bumps automerge today, `cluster/renovate.jsonnet:67-70`) — Renovate runs `nix flake update` and opens one PR through its own PR handling (rebase policy, automerge rules, identity). **It also moves `nixpkgs-ftzmlab` (nas, nuc) to each new NixOS release**, instead of the manual move `flake.nix`'s comment asks for (community NixOS has no long-term branch; each release is supported until about a month after the next): a regex manager on `github:NixOS/nixpkgs/nixos-(?<currentValue>\d{2}\.\d{2})` for that input, datasource `github-tags`, `depName` `NixOS/nixpkgs`, versioning `regex:^(?<major>\d{2})\.(?<minor>\d{2})$` so the `-pre`/`-beta` tags are ignored (nixpkgs tags releases `26.05`, `26.05-beta`, `26.05-pre`). The PR opens when the release is tagged, a month before the old branch dies; a `packageRules` entry sets `automerge: false` for it, so it goes to the triage agent's review mode (release notes against this repo's config, merge or `needs-review`). The `flake.nix` comment is updated to say so. Verify on the first run that all 22 inputs are picked up (all `github:`, which the manager supports). **The post-upgrade render runs in the nix sandbox**, not as `just render-lab` in the job (Workflow security, Autonomy, Rule R): `cluster/flake.nix` exports `packages.x86_64-linux.render-lab`, a `runCommand` with `tanka`, `kubernetes-helm` and `go-jsonnet` that copies the flake's own tree and runs the recipe's `tk export … --skip-manifest` into `$out` plus each `secrets/*.enc.yaml` copied into its namespace's directory; the `render-lab` check compares that output with the committed `cluster/manifests`. `postUpgradeTasks.commands` become `cd cluster && tk tool charts vendor --prune` (unchanged; it fetches charts and executes no repo content) and `cd cluster && nix build --no-update-lock-file --out-link .render-lab "path:.#render-lab" && rm -rf manifests && cp -rL --no-preserve=mode .render-lab manifests && rm .render-lab`, with `RENOVATE_ALLOWED_COMMANDS` matching exactly those; `path:`, not the git reference, because the vendor step leaves new chart directories untracked and a `git+file` flake omits untracked files. Tested on a copy of HEAD's `cluster/`: the build ran in the sandbox, the copied output left `git status` clean (515 files, identical to the committed manifests), and the `path:` flake's tree included an untracked chart directory.
- **`ftzm/triage`: `triage.yml`** (`workflow_dispatch` only, on that repo's runner) — the agent, in a repo whose credentials cannot write `dots` (Workflow security, "Where untrusted content runs"). Secret: `TRIAGE_TOKEN`, mapped only into the trusted gather and publish steps' `env:`. Agent scripts and prompts live in this repo. The agent runs as `claude -p` with `ANTHROPIC_BASE_URL` = the proxy on nuc, Bash as its only tool and every shell command in the sandbox, both set in the VM's managed settings (Workflow security, Containing the agent); its cwd is `agent/`, where it writes its patch, notes and verdict into `out/`, which the publish steps read as data. The trusted gather step assembles the agent's inputs (Workflow security, Autonomy, rule L), then by mode:
  - **fix** — `bin/distil-ci-failure` filters the failed job's log (`GET /repos/ftzm/dots/actions/jobs/{id}/logs`, as the `triage` bot) — ported from `.github/scripts/distil-ci-failure.sh`, whose comment records why (a raw failed log is ~20k lines of chatter; on #147 the agent spent all 40 turns grepping past it); adapted to Forgejo's log line format, magic-nix-cache patterns dropped, prefix handling checked against a real failed Forgejo job log. Then runs the repair agent; the publish step pushes its patch as `triage` by AGit (`refs/for/<renovate-branch>/<topic>`), opening a PR into the Renovate branch; master's `ci.yml` builds it and `merge-repair` acts on it.
  - **review** — runs the review agent with today's prompt (release notes for the full span, each breaking change argued against this repo's config at `file:line`, no manual migration). The publish step posts its notes as a comment on the PR as `triage`, inert (Containing the agent), ending in the verdict line it builds from the agent's verdict file; `triage-verdict.yml` acts on it. It merges nothing.
  - It uploads the transcript to `ftzm/triage`'s own artifacts (the patched `upload-artifact@v4` fork), which follow that private repo's visibility.

## Plan

Nothing changes the source of truth until the manual paths are proven.

1. **Groundwork.** Precondition: `ARGOCD_APPLICATIONS_PLAN.md` is done — met 2026-10-05 (its Sequencing: the bootstrap script and the CI/Renovate render paths below are written against its per-Application layout). First action: turn off sealed-secrets key renewal, then
   back every sealing key up into agenix (Secrets) — done 2026-10-05:
   `keyrenewperiod: '0'` (`578024fc`), the 8 keys in
   `secrets/sealed-secrets-keys.age` (`45868898`). Extend `cluster/scripts/bootstrap` (created by `ARGOCD_APPLICATIONS_PLAN.md` step 6 with its ArgoCD step, a server-side apply as `--field-manager=argocd-controller`) with the sealing-key steps (Cluster Bootstrap, its step 1). Its repo-credential step lands in step 3: the credential is the nas mirror's read key, which step 3 creates, and before the flip ArgoCD reads public GitHub with none.
   Split `cluster/lib/images.libsonnet` into one file per image,
   `cluster/lib/images/<name>.libsonnet` (per-image comments move with
   them), with `images.libsonnet` a fixed map of imports and Renovate's
   regex manager (`cluster/renovate.jsonnet:27`) on
   `/cluster/lib/images/.+\.libsonnet$/`: git conflicts on edits to adjacent
   lines, and the 21 images sit on consecutive lines, so each image-bump
   merge conflicted its neighbours' PRs — which only a Renovate run could
   fix. Separate files never conflict; a Renovate PR then only falls behind
   master, which `update-prs.yml` handles. (`chartfile.yaml` already
   separates its `version:` lines, and each image tag renders into exactly
   one manifest.) `cluster/flake.nix` moves from `nixos-25.11` (EOL since
   2026-06-30; its lock, `2db38e08`, dates from 2026-02-09) to
   `nixos-unstable`: it holds only dev and CI tools (tanka, jsonnet, helm,
   promtool, the `renovate` CLI), which need no stable release, and unstable
   never goes EOL; `lockFileMaintenance` keeps `cluster/flake.lock` current.
   The first update re-renders `cluster/manifests`, one PR whose diff
   shows any output change. Remove
   the `leigheas` peer from `role/network.nix` (the machine no longer exists;
   its `publicKey` `eLpLj1/...` is also eachtrai's, so on every wg host only
   one of 10.0.100.2 / 10.0.100.7 routes). Forgejo: pull-mirror of GitHub
   (temporary) so Actions can be exercised while GitHub stays the source —
   created by hand, like the throwaway repos and users of step 4's
   negative checks: it is test data, deleted in step 3, and the OpenTofu job
   that declares Forgejo's configuration does not exist until then.
   **Step 1 status (2026-10-06): done.** Sealing keys renewal-off and
   backed up; bootstrap restores them before the controller; one file per
   image; `cluster/flake.nix` on `nixos-unstable`; the `leigheas` peer gone;
   the temporary pull mirror created; the day-one checks run (Verified vs.
   Unverified); the self-tests below deployed.
   The manual paths' weekly self-tests (nuc Down → Self-tests) in place
   and green — on saoiste (both self-tests) now; eachtrai's laptop
   self-test is committed with the rest but reports only once eachtrai
   runs a system that has node_exporter (Follow-ups → eachtrai), which does
   not block this plan.
2. **Build farm + agent.** The `dots` runner on nuc (Binary Cache → Design;
   the VM runner becomes the triage runner — re-registered to `ftzm/triage`,
   no longer instance-scoped, host mode with the nixpkgs module, its own store
   disk instead of nuc's store, and the `microvm-egress` nftables table for its qemu process; its
   registration token goes to `mode = "0400"` and is regenerated in Forgejo,
   since it has been world-readable at `0444`; its data image, which holds
   the runner's saved login `.runner`, goes to `0600` via a
   `systemd.tmpfiles` `z` rule (`microvm:kvm`; qemu runs as `microvm`) —
   on nuc it is `-rw-r--r-- microvm:kvm` under world-traversable
   directories, so any local user could extract the login and poll as the
   VM runner, receiving every job it serves with that job's secrets — and
   the re-registration also retires that login); signing +
   harmonia + nginx `/fleet/` on nuc; nuc GC (`min-free`/`max-free`,
   one-time collect); port `ci.yml`; the writer on nuc, polling `dots`
   master **on GitHub** until the flip (it is just a git URL); the
   `fleet-agent` role replacing comin on every host, pi included (Host
   cut-over, under Plumbing). The flip then changes nothing for hosts.
   The writer and agent land with their NixOS VM test
   `checks.x86_64-linux.fleet-deploy` (Binary Cache → Deploy test), which
   passes before any host is cut over.
   Accept (what runs before the flip — PRs still live on GitHub, and the pull
   mirror carries refs only, so the ported `ci.yml` cannot run yet): a host's
   agent deploys a writer-built commit fetching only what it lacks (no eval,
   no compile in its log); a host whose path is unchanged makes no request
   beyond the manifest; a master that fails to build on nuc publishes
   nothing and raises the writer alert; the pi deploys through the agent
   (over LAN, like nas). Cluster side: the comin scrape job and
   `Comin*` rules go; **no new scrape job** — the agent's and the writer's
   gauges are textfile metrics in the directory the existing node_exporter
   scrape already collects; `Fleet*` rules (host lagging the manifest
   commit, `fleet_last_failure`, writer failure, `absent()` per host);
   reboot-need stays `nixos_reboot_required` from `role/node-exporter.nix`,
   with its new `deferred` reason (Plumbing);
   the pi added to that role and the scrape list.
3. **Flip.** Preconditions: the Cluster Bootstrap and Restore Runbook
   rehearsals have passed; Forgejo is its own ArgoCD Application
   (`ARGOCD_APPLICATIONS_PLAN.md`) with the upgrade hooks of Forgejo
   Upgrades, and a deliberately failing upgrade (an image tag whose
   pre-flight fails, then one whose PostSync check fails) has rolled back and
   locked as designed, its retries no-ops (gate stops at the lock, SyncFail
   finds no marker); and a Forgejo sync that fails with no upgrade pending
   (a temporary wave-`0` `Sync` hook Job that exits 1, committed then
   reverted) has neither locked nor rolled back — SyncFail logs the no-marker
   no-op — and the revert syncs normally. GitHub Actions disabled on `ftzm/dots`, so GitHub
   cannot commit once Forgejo is the source (`update-flake-lock.yml`
   auto-merges with a PAT; Renovate's cron merges). The temporary pull-mirror
   repo is deleted and `ftzm/dots` is migrated from GitHub with Forgejo's
   GitHub migrator with `private: true` (`POST /repos/migrate`,
   `MigrateRepoOptions.private` in the live swagger; the GitHub repo is
   public, and the OpenTofu job's `private = true` re-asserts it on the next
   sync) — issues, PRs, labels, milestones, releases, numbers kept,
   so `#246`-style references in commit messages still resolve (GitHub
   Actions run logs do not migrate). Then the **Forgejo OpenTofu job** lands
   (Secrets → Forgejo configuration as OpenTofu): the
   `terraform-provider-forgejo` package, the nix-built image, the PostSync
   hook Job in the Forgejo Application, the `kubernetes` state backend.
   Its first config declares `ftzm/dots` — taken over from the migrator by an
   OpenTofu `import` block, not created — with `private = true`, master's
   protection, the push mirror, the `dots` repo secrets that exist by
   then, and the `monitor` bot with the runner-offline alert (Secrets →
   Runner monitoring); step 4 adds the bot users, `ftzm/triage` and their secrets to it. Renovate and lockfile bumps pause
   until step 4. The writer's poll URL moves from GitHub to the nas mirror (hosts unaffected); the nas
   mirror (git user, keys, `receive.denyNonFastForwards`) and Forgejo's
   `master`-only push mirror to it, with its `last_error` alert; ArgoCD
   `repoURL` → the nas mirror + read key + nas host key (one jsonnet value feeding the `argocd` Application, the operator Applications and the ApplicationSet's generator and template, `ARGOCD_APPLICATIONS_PLAN.md` → Target), and `cluster/scripts/bootstrap` gains the repo-credential step ahead of its ArgoCD apply (Cluster Bootstrap, its step 2);
   every workstation and laptop clone: `origin` → Forgejo, plus a `nas`
   remote; the `Makefile`'s `check-not-behind` changed (Plumbing); `Prune=false` on forgejo
   PVCs. Accept: a push to Forgejo reaches the nas mirror within seconds and
   ArgoCD and the writer both pick it up; a force-push of an older `master`
   to Forgejo is refused by nas and raises the alert; with the forgejo pod at
   zero, a commit pushed to the nas mirror is deployed by ArgoCD. The CI
   half of step 2, now that PRs are on Forgejo: a merged PR's closures come
   from CI (no build on nuc after the merge), and a PR that fails to build
   leaves master, the manifest and every host untouched.
4. **Port the automation**: `renovate.yml` (including the lockfile), the
   `triage` (read on `dots`, no access to `ftzm/triage`), `triage-dispatch`
   (write on `ftzm/triage`, no access to `dots`) and `merger` bot users,
   `ci.yml`'s `dispatch-triage` and `merge-repair` jobs,
   `triage-verdict.yml`, `update-prs.yml`, the `ftzm/triage` repo with
   `triage.yml`, the Claude proxy on nuc and the triage VM's managed Claude
   Code settings (Workflow security, Containing the agent). Accept: one full cycle — flake bump PR opened, CI green from
   cache, auto-merged, every host's `fleet_deployed_commit_info` equal to the
   manifest's commit within minutes, no `Fleet*` alert; one repair cycle — a
   Renovate PR broken on purpose, the triage bot's AGit PR built by master's
   `ci.yml` with no approval, auto-merged into the Renovate branch, the
   Renovate PR then automerged — also when master moved in between (a second
   PR merged first), via `update-prs.yml`. Negative checks: a PR from a
   throwaway non-bot user — as a fork PR, as an AGit PR, and from a branch
   named `renovate/x` — fails `build` at the author check and dispatches no
   triage; a job in a throwaway repo with `runs-on: nixos:host` never
   schedules; a workflow file the `triage` bot adds in its PR waits for
   approval and gets no secrets; `merge-repair` refuses a repair that
   touches a path outside Rule R's allowlist (`.forgejo/`, `bin/`, the
   Renovate config, `cluster/flake.nix`, `cluster/Justfile`, `.claude/`,
   `dotfiles/`, `flake.lock`, a new top-level file); the `triage` bot cannot push a `dots` branch, and gets 403 or 404 on any
   `ftzm/triage` read, push and dispatch;
   `triage-verdict.yml` refuses a `merge` verdict on a stateful component,
   on a PR not authored by `renovate`, and in a comment by anyone but
   `triage`; in a triage job, a sandboxed `curl` to any host fails, a
   sandboxed read of `/var/lib/gitea-runner` and of the runner daemon's
   `/proc/<pid>/environ` fails, from the guest a connection to nuc's `:22`,
   `:5000`, `:6443` and `:10250` and to nas is refused while Forgejo `:443`,
   the proxy `:5002` and DNS work, the agent's environment and the VM hold no Claude
   token, and a checkout whose `.claude/settings.json` sets `enabled:
   false`, `allowUnsandboxedCommands: true`, a wider `allowRead` and an
   allowed domain changes nothing, and the agent has no tool but Bash
   (a `Read` of the runner's `.runner` and a `Write` outside the workdir
   are refused), a sandboxed write to the `ftzm/triage` checkout outside
   `agent/` is refused, and `/tmp/claude-<uid>` is empty after the job; a PR that edits `cluster/manifests` by hand fails
   `render-lab`. One review cycle: a major Renovate PR gets a verdict, and is
   merged or labelled `needs-review` accordingly.
5. **GitHub**: repo deleted (goal 1: no third-party copy), and `.github/`
   deleted from `dots` (ignored by Forgejo since `.forgejo/workflows/`
   exists, but dead code that looks live). Only flake inputs remain.

---

## Binary Cache

### Goals it must satisfy

1. master never moves until the update is proven to build.
2. The closures that proved it are the closures hosts deploy — present in the
   cache **before** the merge commit exists, so the agent substitutes instead
   of building and nothing races.
3. No CI job holds a fleet key, and PR code runs only in the nix sandbox
   or in pure evaluation with `--no-update-lock-file` (Workflow
   security). `dots`' secrets — `merger`, which can reach master,
   among them — reach only jobs on the `dots` runner, which runs only
   trusted code and no other repo's jobs (Workflow security, rule 2).
4. A host closure that is unchanged for months stays cached; the store stays
   bounded.

### Hardware

| Host | CPU | RAM | Disk |
|---|---|---|---|
| nuc | i5-10210U, 4c/8t | 62 GiB (10 used) | NVMe 450G, 144G free (2026-10-05) |
| nas | Athlon 3000G, 2c/4t | 13 GiB (8 used) | root 102G, 34G free; `pool-1` ZFS 3.72T free |
| runner microVM (on nuc) | 2 vcpu | 4 GiB | 20 GiB volume, SLIRP |

nuc is the only always-on x86 box with cores. Closure sizes of the current
tree are unmeasured (none of the four toplevels is in the workstation store);
the first CI build gives them. `nixpkgs-ftzmlab` is `nixos-26.05` (see the
comment in `flake.nix`) and ships harmonia 3.1.0; the numbers in Sizing were
measured on the tree before it moved from 25.11.

### Design

The CI build **is** the cache fill; a writer on nuc turns master into a
signed manifest; a small agent on every host takes its path from the manifest
and activates it. No host evaluates or builds anything.

**Two runners, each registered to one repo** (Workflow security, rule 2).
A repo-scoped runner is invisible to every other repo, so no other repo's
job — whatever `runs-on` it asks for — ever lands on either, and neither
runner's process ever holds another repo's secrets. Other repos' CI, and a
public forge, get their own runner (Later).

- **`dots` runner**: nixpkgs' `services.gitea-actions-runner` on nuc, one
  instance, host mode, label `nixos:host`, registered to `dots` with a
  repo-level registration token (agenix, root-only). Jobs use nuc's daemon
  over the local socket as the module's unprivileged `DynamicUser`
  (`gitea-runner`, nixpkgs `gitea-actions-runner.nix:227-230`) — builds are
  nix-sandboxed on nuc's 8 threads, nuc's store is the cache, harmonia
  serves it. Runs every `dots` workflow: CI, Renovate, `update-prs`,
  `triage-verdict`.
  Every job on it is trusted code: master's workflows (`pull_request_target`,
  `issue_comment`, `push` to master, schedule). PR flakes, for the author
  list only (Workflow security, rule 1), are evaluated in the job purely
  with `--no-update-lock-file`, which reads only store paths and inputs
  locked with their content hash (T15), so neither the job's secrets nor
  the runner's saved login (`.runner` in its state directory) nor files
  earlier jobs left are within their reach. A long-lived runner is therefore enough; nothing here needs a
  runner per job or a network filter.
  Hardening on the unit (systemd overrides): `ProtectSystem=strict`,
  `ProtectHome`, `NoNewPrivileges`, `PrivateTmp`, `MemoryMax`, `CPUWeight`.
  forgejo-runner is the `nixpkgs-ftzmlab` package (13.2.0,
  `pkgs/by-name/fo/forgejo-runner/package.nix:30`) on both runners and
  follows its bumps.
- **Triage runner**: the existing microVM (SLIRP, small), re-registered
  from instance scope to `ftzm/triage`, running jobs in **host mode** in the
  guest with nixpkgs' `services.gitea-actions-runner` — the same module as
  the `dots` runner — instead of containers: Claude Code's sandbox is
  bubblewrap, which inside an unprivileged container needs
  `enableWeakerNestedSandbox`, described by Claude Code itself as weakening
  isolation. It runs only `triage.yml` — arbitrary shell steered by
  untrusted text (the LLM) — so the VM holds only `ftzm/triage`'s automatic
  token and the `triage` bot's token (read on `dots`, no access to
  `ftzm/triage`); the Claude token stays on nuc. Walls: the Claude Code
  sandbox around every agent command (Workflow security, Containing the
  agent), then the VM:
  - **No host store.** The `ro-store` share of nuc's `/nix/store`
    (`forgejo-runner.nix:60-67`) goes; microvm.nix then boots the guest from
    a store disk of its own closure (`storeOnDisk` defaults to true when no
    share maps `/nix/store`, `nixos-modules/microvm/options.nix:589-594`).
    nuc's store holds every evaluated `dots` source tree and everything
    nuc builds, which guest root must not read.
  - **Port-level egress filter for the qemu process.** SLIRP traffic
    leaves nuc from the qemu process, which runs as `microvm`
    (`User=microvm` on `microvm@forgejo-runner.service`), so a host-side
    filter on that uid holds even against guest root. systemd's
    `IPAddressAllow=`/`IPAddressDeny=` cannot be the filter: they match
    addresses only, and nuc (firewall off, `machines/nuc/default.nix:118`)
    listens on every address with, among others, sshd `:22`, NFS `:2049`,
    rpcbind `:111`, mosquitto `:1883`, k3s-server `:6443` and `:10250`,
    node_exporter `:9100`, comin `:4243`, jellyfin `:8096`, and on
    `192.168.1.4` etcd `:2379`/`:2380` and smbd `:139`/`:445` (`ss -tlnp`
    on nuc) — plus harmonia `:5000`, which serves the very store the
    `ro-store` share was removed to hide. So nuc's NixOS config declares one
    `networking.nftables.tables` table, `inet microvm-egress`, chain
    `output` (hook output: SLIRP's sockets are nuc's own), policy accept,
    rules for `meta skuid "microvm"` only:
    - accept tcp `443` to `192.168.1.4` and `100.64.0.2` (Forgejo via
      traefik; the https name resolves to the Tailscale one);
    - accept tcp `5002` to `192.168.1.4` (the Claude proxy);
    - accept udp/tcp `53` to `100.100.100.100` — SLIRP answers the guest's
      DNS by forwarding to nuc's resolver, which is MagicDNS
      (`nameserver 100.100.100.100` in nuc's `/etc/resolv.conf`);
    - drop everything else to `10.0.0.0/8`, `172.16.0.0/12`,
      `192.168.0.0/16`, `100.64.0.0/10`, `127.0.0.0/8` (the LAN, wg, the
      tailnet, nuc's loopback, which SLIRP otherwise exposes), and all IPv6
      (every tailnet host also has an address in headscale's default
      `fd7a:115c:a1e0::/48`, nixpkgs `headscale.nix:160`).
    Plus `RestrictAddressFamilies=AF_UNIX AF_INET` on the qemu unit.
    Enabling `networking.nftables` is safe on nuc: with `tables` only and
    `stateVersion` `24.11` the module does not flush the ruleset, only
    deletes and recreates its own tables (nixpkgs `nftables.nix:280-310`);
    it blacklists the `ip_tables` module, which nuc does not use — k3s's
    rules are on `nf_tables` via `nft_compat`, and
    `/proc/net/ip_tables_names` does not exist. The module checks the
    ruleset at build time in the nix sandbox, where `microvm` does not
    exist ("Error: User does not exist" fails `nftables-rules.drv`), and
    `microvm` has no fixed uid (980 on nuc, allocated by NixOS;
    microvm.nix `nixos-modules/host/default.nix:81`), so the rules name the
    user and `networking.nftables.preCheckRuleset = "sed 's/skuid
    \"microvm\"/skuid root/g' -i ruleset.conf"` substitutes a name that
    exists for the check only (the module's documented use,
    `nftables.nix:110-121`). No `ct state` rule is needed: every packet
    qemu sends on an allowed flow has the allowed destination and port, and
    the listeners' replies are not `microvm`'s. Tested in a NixOS VM on
    `nixpkgs-ftzmlab` with root-owned listeners: as `microvm`, `:443` on
    both addresses, `:5002` on `192.168.1.4` and `:53` on `100.100.100.100`
    connect, `192.168.1.4:22`/`:6443`, `100.64.0.2:5002` and
    `127.0.0.1:22` are blocked; root reaches all of them. Internet egress stays open
    for the trusted gather steps and Claude Code's own API traffic; the
    agent's commands have no network at all (Containing the agent).

**Trust model:** master is trusted. The required status check is not a
security gate (Workflow security), so what bounds who can move master is who
holds a token that merges, and what runs where that token is:

- Merge-capable tokens: the owner; `renovate` and `merger`, both `dots`
  secrets, which reach only jobs on the `dots` runner. Those jobs run
  workflow code from master, or from branches the owner or Renovate pushed.
  The `triage` bot pushes no `dots` branch.
- PR code: pure nix evaluation with `--no-update-lock-file`, for the
  author list only; everything else in the nix sandbox. A PR's own workflow files never
  run without approval (bot and stranger PRs) or are the owner's and
  Renovate's.
- Upstream-authored text: read only in `ftzm/triage`, on its own runner.
- What can therefore reach master: as listed in Workflow security.

Beyond that: nuc's daemon guarantees that a signed path is an honest build of
its derivation; the writer guarantees hosts run only what is on master.
Nothing that runs in a job holds the cache or manifest keys, and no job
writes the manifest or starts the writer: it polls the nas mirror itself.

- **`dots` runner's daemon access**: the `gitea-runner` user is **not** in
  `trusted-users`; the daemon honours `max-jobs`, `cores`,
  `use-substitutes`, `keep-failed` from any client including a job passing
  `--option`, so those are defaults, not bounds. nuc protects itself with the
  unit's cgroup limits and a `min-free` large enough to keep k3s and
  Forgejo's SQLite alive on the shared NVMe; a job can still make nuc slow or
  fill it — accepted.
- **Signing** by nuc's daemon: `nix.settings.secret-key-files` → agenix,
  root-only, never in the VM. Every path nuc builds is signed. A signature
  attests "this derivation, built in the sandbox, produced these bytes". An
  untrusted client can also `nix store add` arbitrary bytes and get them
  signed, but those are content-addressed paths and cannot impersonate an
  input-addressed one; a PR cannot obtain a signed fake of a path master
  will name.
- **Serving**: `services.harmonia` on nuc (3.1.0 in `nixpkgs-ftzmlab`:
  `services.harmonia.enable` + `cache.signKeyPaths`; socket-activated
  (`harmonia.socket`), reads `/nix/var/nix/db/db.sqlite` directly, no
  nix-daemon connection; the separate `services.harmonia.daemon` is not
  needed for serving), on its default `bind = "[::]:5000"` — every address
  (`harmonia.nix:113`). Next to it, nginx serves one static directory,
  `/fleet/`, on port `5001` of every address. Nothing binds to a specific IP,
  so nothing waits for tailscaled at boot; the LAN is trusted and nuc's
  firewall is off (`machines/nuc/default.nix:118`). Not port 80:
  traefik (`hostNetwork: true`) holds `:80`/`:443` on all three of nuc's IPs
  (`traefik-deployment.yaml:41-42,61,64,65,68`) plus `:8080`, `:9091`,
  `:6881` on every address. The agent fetches
  `http://<ip>:5001/fleet/manifest`. Transport per Decisions: LAN for nas and
  the pi, Tailscale for laptops, no wg.
- **Writer** (`fleet-write`, on nuc, dedicated `fleet-writer` user, holds a
  read-only key for the nas mirror and nothing else):
  - **Trigger.** A 1-minute timer runs `git ls-remote` against the nas
    mirror (before the flip: GitHub) and builds when `master` differs from
    the published commit. Nothing starts it from CI. A merge reaches hosts
    within about a minute plus the mirror push plus one agent tick; a
    restored nuc publishes on its own.
  - **No rewind check of its own.** nas `master` cannot move backwards
    (`receive.denyNonFastForwards`, Decisions), so every head the writer sees
    descends from the last one; a rollback is a revert commit.
  - **One build, one profile.** `flake.nix` exports
    `packages.x86_64-linux.fleet = pkgs.linkFarm "fleet" (mapAttrs (h: c:
    c.config.system.build.toplevel) self.nixosConfigurations)` — a directory
    of one symlink per host to its toplevel, whose closure is every host's
    system (the pi's aarch64 toplevel is a valid reference; nuc has binfmt).
    CI builds that one attribute; the writer runs `nix build --no-link
    --no-update-lock-file --print-out-paths
    "git+file://$repo?rev=$sha#fleet"` (the flag: exactly the committed
    lock, as CI evaluates it, Workflow security; the git fetcher builds
    exactly that commit from the object store; a `path:` reference would copy
    the working tree, ignoring `rev` and including untracked files) (a no-op when CI built it
    pre-merge; a real build on nuc when something merged without CI: the
    manifest only advances once every closure exists, and a failure for any
    host publishes nothing).
  - If the resulting path equals what the `fleet` profile already points at,
    **nothing changed: only the commit stamp is updated — no
    host does anything** (a README-only commit yields the identical linkFarm
    path).
  - Else `nix-env -p /nix/var/nix/profiles/per-user/fleet-writer/fleet --set <path>`: generation N
    is now a GC root for all five toplevels, generation N−1 still roots the
    previous set (`--delete-generations +2` keeps exactly those two); render
    `{generated, commit, hosts:{host:{path, system, commit}}}` with
    `path` from `readlink` on the linkFarm entries and `system` from
    `<toplevel>/system` (NixOS writes it); **make the manifest itself a store
    path**: `m=$(nix store add --mode flat manifest.json)`, root it with a
    second profile (`nix-env -p /nix/var/nix/profiles/per-user/fleet-writer/fleet-manifest --set
    "$m"`), and atomically write that one store path to
    `/var/www/fleet/manifest` (`mv` of a temp file). Harmonia serves the
    manifest like any other path and nix verifies it on the host — **one
    trust root, the cache key hosts already carry**; the pointer is one line
    in one file, so nothing straddles. No hand-managed roots anywhere: nix's
    profile mechanism is the retention policy, and there is nothing to
    re-register.
  - Sequential by construction (one service, one lock). It always describes
    master's head.
  - **Rollback is a revert commit on master** — CI is instant (the closures
    are still rooted by generation N−1), the writer publishes, hosts switch.
    The writer has no option to pin an older commit, so master stays the only
    source of truth.
  - History = the profile's generations (`nix-env -p …/fleet
    --list-generations`), each timestamped, plus the manifest's `commit`.
  - Metrics to nuc's textfile dir: `fleet_writer_last_success_timestamp`,
    `fleet_writer_last_failure` (set from an `ERR` trap),
    `fleet_manifest_commit_info{commit}`.
  - There is no per-host testing mechanism: NixOS is atomic and a bad merge
    is a revert commit. A manual switch on a host is undone by the agent's
    next tick; when one must persist, stop the timer (nuc Down → Keeping a
    manual switch).
- **Agent** (`fleet-agent`, a NixOS role on every host including the pi): a
  systemd timer (2 min) running a short script:
  - One `curl --max-time 20` of the pointer `manifest` (a store path);
    `nix-store --realise` it — harmonia serves it, nix checks the signature
    against `trusted-public-keys`; read its own entry from the file in the
    store. The agent keeps no state between ticks beyond the failure flag
    below.
  - If the path equals `/run/current-system`: done, **no other network
    access** — unless the last switch to that path failed, in which case the
    failure stays reported (sticky) until it is cleared (below). The same
    holds when the path equals the `system` profile's target
    (`readlink -f /nix/var/nix/profiles/system`) and no failure is flagged:
    that is a deferred switch (below) waiting for a reboot, and the agent
    does not re-run it every tick.
  - **The failure flag** is a file, `/var/lib/fleet-agent/failed`, holding
    the store path whose activation failed. A failure can leave
    `/run/current-system` already pointing at that path: the activation
    script links it at its end (`activation-script.nix:79`), before
    `switch-to-configuration` stops, starts and restarts units, so a
    `switch` that fails or times out in the unit phase leaves the path
    "current", and a reboot boots it in full (the boot entry is already
    installed). The agent retries such a path once, 30 minutes after the
    failure (decided 2026-10-06; `/var/lib/fleet-agent/retried`): a
    transient — nuc's first cache deploy exited 4 because a user session was
    closing mid-switch — clears on its own; a broken unit or a hang costs one
    more bounded attempt and then stays reported. The flag is cleared, and
    `fleet_last_failure` set to 0, when a later activation succeeds or is
    deferred, or when `readlink -f /run/booted-system` equals the flagged
    path: the host has booted the system that failed to switch, which
    completes the switch. Without that last rule the host reports a failure
    while running the right system, until a new commit changes the path.
  - Otherwise `nix-store --realise <path>` (ordinary substitution, signatures
    checked by nix, substituters in order: cache.nixos.org for what it has —
    the bulk, over the CDN even for a laptop off-LAN — then harmonia; there
    is no `.drv` to fall back to, so `fallback` is irrelevant here),
    `nix-env -p /nix/var/nix/profiles/system --set <path>`, then two
    activations, each `systemd-run --wait --pipe --collect
    --service-type=exec --unit=fleet-agent-switch -p RuntimeMaxSec=15min
    <path>/bin/switch-to-configuration <action>` — in its own transient
    unit, as `nixos-rebuild` does
    (`--unit=nixos-rebuild-switch-to-configuration`, nixpkgs
    `nixos-rebuild-ng/src/nixos_rebuild/nix.py:40-52`), so a commit that
    changes `fleet-agent.service` does not kill the switch it triggers:
    1. **`boot`** first. It installs the boot entry and activates nothing,
       so a `switch` that hangs or dies part-way still leaves the new
       generation as the boot default — the failure comin issue #200
       describes (a stuck `sysinit-reactivation.target`, no boot entry
       written, the host not upgradable even by reboot). `boot` skips the
       switch-inhibitor check (`switchable-system.nix`'s
       `preSwitchChecks.switchInhibitors` exits early for `boot`).
    2. **`switch`**, unless the **switch inhibitors** differ. nixpkgs writes
       `system.switch.inhibitors` into every system as `$out/switch-inhibitors`
       (JSON), and its pre-switch check refuses a `switch` (exit 1, before
       the bootloader step, `switch-to-configuration-ng/src/main.rs:1829-1845`)
       when a key present in both `/run/current-system/switch-inhibitors` and
       the new file has a different value. The agent runs the same jq
       comparison first — keys in both, value changed, a missing file read
       as `{}` — and on a difference skips the `switch`. The one inhibitor
       nixpkgs sets is `dbus-implementation` (`services/system/dbus.nix:119`;
       this repo sets none); without the comparison a `dbus` → `broker`
       change fails the `switch` every tick forever, the stall comin issue
       #155 reported. comin does the same since PR #156 (`computeOperation`,
       `internal/store/deployment.go:408-420`: `switch` becomes `boot`).
       A `switch` that exits **100** is the same case:
       `init-interface-version` differs (the new systemd's interface is
       incompatible with the running PID 1, `main.rs:1863-1878`), the boot
       entry is installed and nothing was activated.

    An inhibitor difference or exit 100 is a **deferred switch** and counts
    as success, not failure: `fleet_last_failure` cleared,
    `fleet_deployed_commit_info` set to the new commit (it is the installed
    boot default), generations pruned as after a switch. The host runs the
    new generation from its next reboot; the pending reboot is reported by
    `nixos_reboot_required{reason="deferred"}` (Plumbing). On the
    always-on hosts the agent reboots into it (`fleetAgent.autoReboot`,
    decided 2026-10-06): a nightly timer, staggered so dependent hosts never
    reboot together — nas 04:00, nuc 04:30, pi 05:00 — reboots when the
    system profile differs from what runs and no activation failed, once per
    installed path (`/var/lib/fleet-agent/rebooted-for`), so a system that
    does not come up as itself is not rebooted into again; the timer is not
    `Persistent`, so a host that was off at the window does not reboot on
    boot. Laptops keep reboots manual, reported by
    `DeferredSwitchNeedsReboot`. Any other non-zero exit from either
    activation is a failure.

    **`RuntimeMaxSec=15min` bounds each activation.** Without it a hung
    activation blocks `systemd-run --wait` forever; `fleet-agent.service`
    stays active, so its timer never starts it again, and the host stops
    deploying with nothing reported. On expiry systemd stops the unit
    (`Finished with result: timeout`, SIGTERM) and `systemd-run` exits 1
    (checked with a `--user` unit and `RuntimeMaxSec=2`), which the agent
    records as a failure. Because `boot` ran first, a timed-out `switch`
    still leaves the new generation as the boot default. 15 min is about 12×
    the longest comin `switch` in the last 60 days of journals (from comin's
    "running … switch-to-configuration" to "switch successfully terminated":
    nuc 76 s, nas 37 s, saoiste 22 s). The pi has no comin history; its
    first deploys under the agent are the measurement, and the bound is
    revisited if one comes within a factor of 3.
  - Three node_exporter textfile gauges in
    `/var/lib/prometheus-node-exporter-text-files/` (the dir the role
    collects, scraped already): `fleet_deployed_commit_info{commit}`,
    `fleet_last_success_timestamp`, `fleet_last_failure`.
  - A host that is off or off-LAN simply retries next tick. A manual
    `nixos-rebuild` on a host is **reverted on the next tick**; to keep one,
    stop the timer.
- **Hosts** need: the role (agent, `trusted-public-keys` with nuc's cache
  key — the only key, `substituters` order, `connect-timeout = 5`, and
  **their own GC** — every manifest change is a new generation via `nix-env
  --set` on every host: after each successful or deferred switch the agent runs
  `nix-env -p /nix/var/nix/profiles/system --delete-generations +N` — N = 5,
  N = 3 on the pi, which has a 29 GB SD with 20 GB free and a 5.1 GiB closure
  — and a weekly `nix.gc` without an age option frees the paths, plus
  `min-free`. `nix.gc` alone cannot keep a count: `nix-collect-garbage` only
  has `--delete-older-than`. N ≥ 2 keeps the previous generation as the boot
  menu's rollback target (S2)). Nothing else: no git, no deploy keys,
  no experimental features.

### Plumbing

- **`dots` runner's tools**: its `hostPackages` (`git`, `nix`, `jq`,
  `curl`, `just`, `nvd`, `openssh`); `nix develop ./cluster`
  for Renovate works natively — same machine as the store.
- **nginx `/fleet/`** listens on `:5001` of every address (nuc's existing
  nginx vhost is on `127.0.0.1:8085` only, `nuc/default.nix:67-72`); the PR
  job on nuc reads the current pointer locally (`/var/www/fleet/manifest`)
  for `nvd diff`.
- **The pi's monitoring**: the pi added to `role/node-exporter.nix` hosts and
  to the scrape list in `lab.jsonnet` by LAN IP (`alwaysOn: true`).
- **Runner registration**: each runner registers to its one repo with that
  repo's registration token (`GET /repos/{o}/{r}/actions/runners/registration-token`,
  `routers/api/v1/api.go:501`), fetched once by the owner into agenix: the
  `dots` runner's on nuc, the triage runner's into the VM (Secrets). Builds
  serialise on nuc's daemon regardless of how many jobs run.
- **Actions on a pull-mirror repo** (checked 2026-10-06, Forgejo v16.0.5
  source): they run once the repo's Actions unit is on — a mirror sync emits
  an ordinary `push` (`SyncPushCommits`, `services/actions/notifier.go:646-679`),
  gated only by that unit (`notifier_helper.go:155,163`); nothing in
  `services/actions` checks `IsMirror`. Migrated mirrors start with the unit
  off. The temporary `ftzm/dots` pull mirror (created 2026-10-06, private,
  10-minute interval) keeps it off until `.forgejo/workflows/` exists in dots
  — from then on Forgejo ignores `.github/`, so enabling it runs only the
  ported workflows, never GitHub's Renovate and flake-update schedules on the
  instance runner.
- **nuc's daemon substituters**: the caches in `flake.nix`'s `nixConfig`
  (`nixos-raspberrypi.cachix.org`, `claude-code.cachix.org`, `pi.cachix.org`
  while it lasts) are client-side settings that an untrusted client cannot
  pass to the daemon. They go into nuc's `nix.settings.substituters` +
  `trusted-public-keys`, or the pi's kernel gets built under emulation. The
  sizing numbers assume those caches. Measured 2026-10-06: the writer's
  first cold build of `.#fleet` on nuc (1246 derivations, 10.4 GiB fetched)
  took 30 minutes, pi included, Iosevka prebuilt.
- **`boot.binfmt.emulatedSystems = ["aarch64-linux"]` on nuc**, which also
  sets `extra-platforms`; nothing sets it today.
- **Writer credentials**: a read-only key authorized on the nas mirror and
  nas's committed host public key in the writer's `known_hosts` — a nuc-side change at
  the flip whose failure mode is "no manifest, no deploys"; recovery is the
  workstation push to nuc. Flake inputs still come from github.com until they
  are in nuc's store, so a lockfile bump cannot deploy while GitHub is
  unreachable.
- **nuc deploys itself with no canary**, as under comin: its own agent's
  `switch` restarts `nix-daemon.service` and `harmonia.socket`, which kills a
  build in flight; the writer's run fails, reports it, and the next tick
  retries.
- **`nixos_reboot_required` gains `reason="deferred"`** in
  `role/node-exporter.nix`: set when `readlink -f
  /nix/var/nix/profiles/system` differs from `readlink -f
  /run/current-system`, checked before the kernel and initrd comparisons
  (a reboot applies everything). Today `reason` is only `kernel` or
  `initrd` (`role/node-exporter.nix:41-45`), and a deferred switch moves
  neither the booted nor the current kernel, so without this the metric
  stays 0 on a host that needs a reboot to run what it has installed.
- **Manual deploy check**: every `Makefile` deploy target runs
  `check-not-behind` (`Makefile:1-7`), which today does `git fetch origin`
  and aborts when that fails — during a Forgejo or nuc outage, exactly when
  the manual path is needed. It becomes: fetch `origin` (Forgejo) and `nas`,
  each failure tolerated; refuse if `HEAD` is behind either one it reached;
  if neither is reachable, print a warning and continue. The check is right
  whichever copy lags, and the escape hatch never depends on the
  infrastructure it bypasses.
- **Claude proxy** (Workflow security, Containing the agent): Envoy on nuc
  (`services.envoy`), plain HTTP on `192.168.1.4:5002`, one route: `POST`
  on `/v1/messages` and `/v1/messages/count_tokens`, forwarded over TLS to
  `api.anthropic.com` with the OAuth token added by Envoy's
  `credential_injector` filter from an agenix file (overwriting whatever
  `Authorization` the client sent); every other path is refused. The triage
  VM sets `ANTHROPIC_BASE_URL` to it. Reachable from the LAN, which is
  trusted.
- **Addresses come from the inventories, never literals.** Every IP in this
  plan (`192.168.1.4`, `100.64.0.2`, `192.168.1.3`) names an entry of
  `role/lab.nix` (Nix, the `lab` module argument) or
  `cluster/lib/config.libsonnet` (jsonnet); a tailnet re-registration changes
  a Tailscale IP, and then only the inventory changes. `role/lab.nix`
  `services` gains `fleetManifestLan`, `fleetManifestTailscale`
  (`http://<nuc>:5001/fleet/manifest`), `fleetCacheLan`,
  `fleetCacheTailscale` (`:5000`), `nasMirror`
  (`ssh://git@<nas>/dots.git`) and `claudeProxy`
  (`http://<nuc lan>:5002`), next to its existing `lokiPush`. The
  `fleet-agent` role imports `role/lab.nix` (today only nuc and nas do) and
  takes its URLs from `lab.services`; the `microvm-egress` table takes its
  addresses from `lab.machines`; ArgoCD's `repoURL` comes
  from `config.libsonnet`'s nas entry.
- **Host cut-over (step 2)**, two commits, no hand pushes but the pi's
  (decided 2026-10-06, replacing one commit pushed by `make <host>` to every
  host):
  1. **Commit A** adds `fleet-agent` (`role/fleet-host.nix`) to every host.
     On the comin hosts (nas, nuc, saoiste, eachtrai) comin deploys it, and
     the agent runs in `dryRun` (`role/comin.nix` sets it): it fetches and
     verifies the manifest and downloads the closure but activates nothing,
     since comin switches to a commit minutes before the writer publishes it
     and a live agent would switch back meanwhile. A also gives comin's unit
     `X-StopOnRemoval=false`: `switch-to-configuration` stops a removed unit
     only if its *current* unit says so (`switch-to-configuration-ng`
     `main.rs:1248`), so the later removal of comin cannot kill the switch
     comin itself runs. The pi, which has no deployer, gets its first agent
     system by one push from saoiste (`boot`, then a reboot: its kernel and
     dbus implementation change).
  2. **Gate:** every comin host *runs* A — `fleet_last_success_timestamp`
     from it in Prometheus, i.e. its running system has the agent. eachtrai
     needs a reboot for that (Follow-ups).
  3. **Commit B** drops `role/comin.nix` (and with it `dryRun`) and the comin
     scrape job and `Comin*` rules. comin applies it; its unit is left
     running by the guard and a oneshot stops it once it has no switch in
     flight. From then on the agents deploy.
  The `Fleet*` rules, the pi's node_exporter and scrape target, and
  `fleet_tailnet_peer_online` (nuc's view of the laptops on the tailnet,
  replacing comin's exporter as `RoamingNodeExporterDown`'s evidence that a
  laptop is up) land with A.

### Deploy test

`checks.x86_64-linux.fleet-deploy`, a NixOS VM test in the repo, built by CI
like every flake check, so any PR touching the writer or agent re-runs it.
The scratchpad harnesses T13/T14 tested an earlier writer and agent and are
not reused. Nodes: `nuc` — the real writer, harmonia and nginx `/fleet/`, a
local bare repo standing in for the nas mirror; `host` — the real agent
role. Cases:

- first publish; the host substitutes only the changed paths and switches;
- a README-only commit publishes nothing;
- a revert commit republishes the older path and the host follows;
- an unchanged host makes no request beyond the manifest;
- a commit that fails to build publishes nothing and sets
  `fleet_writer_last_failure`;
- a commit changing `fleet-agent.service` completes its own switch
  (`systemd-run`);
- a commit changing `services.dbus.implementation` installs the boot entry
  and does not switch: `/run/current-system` unchanged,
  `nixos_reboot_required{reason="deferred"}` 1, `fleet_last_failure` 0;
  the next tick makes no request beyond the manifest and runs no
  activation; after a reboot of the node the host runs the new path and the
  reason clears (the `host` node needs `virtualisation.useBootLoader`, so a
  reboot boots the installed entry);
- a commit whose `systemd.package` reports a different `interfaceVersion`
  makes `switch` exit 100 and is handled as deferred, with the same
  assertions. The commit sets `systemd.package = pkgs.systemd.overrideAttrs
  (o: { passthru = o.passthru // { interfaceVersion = 99; }; })`: passthru
  is not part of the derivation, so nothing rebuilds (checked on the
  `nixpkgs` input: equal `drvPath`, `interfaceVersion` 99 vs 2);
- an activation that exceeds its bound is stopped and reported as a
  failure, and the timer starts the agent again on the next tick (it is not
  left blocked): the test runs the agent with a
  short `RuntimeMaxSec` against a commit adding a `Type=oneshot` unit,
  `wantedBy = ["multi-user.target"]`, whose `ExecStart` sleeps past it —
  the hang falls in the unit phase, after `/run/current-system` is linked;
  the next tick makes no request beyond the manifest and runs no
  activation, `fleet_last_failure` stays 1; after a reboot of the node
  `/run/booted-system` is the flagged path and the flag clears
  (`fleet_last_failure` 0);
- GC under `min-free` pressure keeps both profile generations' closures.

Needs the `kvm` system feature on nuc: present (`/dev/kvm` exists and
nuc's `system-features` are `benchmark big-parallel kvm nixos-test`).

### Flow

1. Flake-bump PR opens. Master's `ci.yml` (`pull_request_target`) runs on
   the `dots` runner: `nix build` every host's toplevel and the
   `render-lab`/`test-rules`/`mailsort` flake checks, all built and signed by
   nuc's daemon in the sandbox; only evaluation runs in the job. PR closures
   carry no roots and survive on nuc until pressure. `nvd diff` per host (the
   current path comes from the published manifest) posted as the PR
   comment.
2. Build fails → PR stays open, nothing else moves. Build passes → auto-merge.
3. master moves; Forgejo pushes it to nas. The writer's next poll sees it;
   it evaluates on
   nuc, finds every closure already built, publishes the manifest, advances
   the profile.
4. Each host's agent sees a new path for itself on its next tick, substitutes
   only what it lacks, switches. The pi included.

### GC

nuc's store, nuc's GC, nix's own collector: `min-free`/`max-free` on
pressure, no schedule, no `--delete-older-than`, **no `keep-outputs`** (it
would retain the build-time closure of five systems to protect one package —
Iosevka — whose rebuild trigger is a manual pin bump; `keep-derivations`
stays at its default). Roots are two nix profiles on nuc, both written only
by the writer:

| Profile | Points at | Retention |
|---|---|---|
| `/nix/var/nix/profiles/per-user/fleet-writer/fleet` | the `.#fleet` linkFarm — every host's toplevel | `--delete-generations +2`: the published set and the one before it (covers a host mid-substitution and a one-step revert) |
| `/nix/var/nix/profiles/per-user/fleet-writer/fleet-manifest` | the manifest store path | the same |

The profiles live in `/nix/var/nix/profiles/per-user/fleet-writer/`, nix's
convention for a non-root user's profiles (`/nix/var/nix/profiles` itself is
`root:root 0755`); a `systemd.tmpfiles` rule creates the directory owned by
`fleet-writer`. Nix scans `/nix/var/nix/profiles` for roots; nothing is
registered by hand.
PR-build closures carry no roots and live until pressure.

A missing path is a **performance** event, never a blocked host: the writer's
`nix build` rebuilds it on nuc (the build farm) before publishing; if
cache-hit rate on rebased PRs ever matters, root PR builds then.

### What it gives

- Every machine, the pi included, runs exactly what CI proved, without
  evaluating or building. A host whose path is unchanged makes no network
  call beyond the manifest.
- nuc down: nothing new deploys; every host keeps running and retries.
  Laptops off-LAN: deploys work over Tailscale, slower. Manual paths
  (workstation push, laptop self-rebuild) are unaffected and converge on the
  next manifest.
- Deploys within about a minute + one agent tick when CI built the closure (always, with
  `block_on_outdated_branch` and the unfiltered build job); rollback as a
  revert commit; history as profile generations; a no-change commit moves
  nothing.
- Alerting on three gauges per host, the writer's own, and the existing
  reboot metric.

### Sizing

Closures, from `nix build --dry-run` against an empty store with the flake's
substituters (what a cold store must download / build):

| Host | Download | Unpacked | Must build |
|---|---|---|---|
| saoiste | 7.5 GiB | 22.0 GiB | 532 drvs |
| eachtrai | 5.7 GiB | 18.4 GiB | 564 |
| nuc | 2.6 GiB | 7.9 GiB | 597 |
| nas | 1.7 GiB | 3.6 GiB | 321 |
| pi (aarch64) | 2.0 GiB | 5.1 GiB | 345 |
| **four x86 hosts together (deduplicated)** | **11.4 GiB** | **35.8 GiB** | 1507 |

≈ 41 GiB of outputs per fleet generation. The "must build" lists are almost
entirely config glue (units, etc files, initrds, module-shrinking, FHS
wrappers for zoom/steam, the emacs-with-packages wrapper, vterm, treesit
grammars). Emacs core and every kernel are substitutable. The one heavy
from-source build was **`Iosevka-ftzm`**; since 2026-10-06 it lives in its
own repo, `github.com/ftzm/iosevka-ftzm`, whose release workflow builds it
and attaches the TTFs, and dots fetches that asset by hash
(`pkgs/iosevka-ftzm.nix`; Renovate bumps it, `nix-update --version skip`
rewrites the hash) — no host or CI here builds it. The pi is built with
`nixos-raspberrypi.lib.nixosSystem` (not `nixosInstaller`, whose global
ffmpeg overlay rebuilt a python test chain under emulation); its
kernel/firmware come from `nixos-raspberrypi.cachix.org`, the rest of it
from cache.nixos.org, and emulation covers only glue (69 derivations).

| Resource | Now | Target | Why |
|---|---|---|---|
| Triage runner VM | 2 vcpu / 4 GiB / 20 GiB, SLIRP, container labels, host store share, instance-scoped | same CPU/RAM; own store disk, no host store share; the `microvm-egress` nftables table on nuc for its qemu process; host mode, registered to `ftzm/triage`; the 20 GiB volume must hold the agent's workdir store (`dots`' flake inputs, the cluster dev shell) — size measured on the first run | it builds nothing beyond evaluation and `render-lab`; the triage agent only |
| `dots` runner unit on nuc | — | `MemoryMax` ~24 GiB, `CPUWeight` below k3s; evals run here (five sequential, ~1.5–2 GiB each) | 62 GiB RAM, 10 used |
| Host store (nuc `/nix`) | 135 GiB used (last measured before 2026-10-05), 144 GiB free on `/` (2026-10-05), no GC | fleet closures live here: ~41 GiB per generation, ~80 with generation N−1 during a bump, +~20 for PR builds kept opportunistically; `min-free` 20 GiB / `max-free` 60 GiB | the store nuc already has; the 41 GiB overlaps nuc's own 8 GiB closure |
| Host CPU/RAM for builds | — | all 8 threads, whatever RAM Iosevka needs, alongside k3s (load average ~2) | builds are bursty; the runner unit's limits bound the job side |
| Network | — | harmonia (`:5000`) + nginx (`:5001`) on every nuc address; nas and pi over LAN, laptops over Tailscale | no SLIRP anywhere in the build or serving path |

Prerequisite on nuc: its `/nix` is **135 GiB for an 8 GiB system** and nothing
enables `nix.gc` on any host. One-time `nix-collect-garbage` (unrooted paths
only — nuc's generations are rooted by its profile; **no**
`--delete-older-than`, those generations are the S2 recovery target) returns
the unrooted part; then `min-free`/`max-free` keep it bounded. Without
`keep-outputs` the resident set is the table's numbers plus one previous
generation.

### Later

- **Shared runner, when other repos need CI** (and before the instance opens
  to the public): instance-scoped, one fresh microVM per job with a
  single-use registration (`POST /api/v1/admin/actions/runners` with
  `ephemeral: true`, `routers/api/v1/api.go:1371-1374`,
  `modules/structs/runner.go:8-31`; runner `one-job`,
  `internal/app/cmd/cmd.go:65-77`), so a compromised job ends with its VM
  and never reaches a later job's credentials; the triage VM's
  `microvm-egress` nftables table. Each VM boots from a store disk of its
  own, like the triage VM (no `ro-store` share of nuc's `/nix/store`), with
  no nix-daemon socket of nuc's and no route to harmonia `:5000` (the
  `microvm-egress` table already drops it): a public job is a stranger's
  code, and nuc's store holds every evaluated `dots` tree, while nuc's
  daemon signs whatever it builds and harmonia serves that to every host.
  `dots` and triage never run on it (Workflow security, rule 2).

## Secrets

Rule: every private half lives in exactly one system — **agenix** for NixOS
hosts and the runner VM (delivered into the VM the way the runner token is
today: decrypted on nuc into a dedicated dir, `symlink = false`, shared by
virtiofs), **Sealed Secrets** for pods. Every public half is committed in
plaintext. Everything Forgejo has to be *told* (deploy keys, repo secrets,
branch protection, the bot users, the push mirror) is declared in OpenTofu
and applied from k8s (below), because Forgejo has no config-as-code for repo
settings.

| Secret | Private half | Public half / consumer | Applied by | Rotation |
|---|---|---|---|---|
| Forgejo `SECRET_KEY`, `INTERNAL_TOKEN`, `JWT_SECRET`, admin creds | SealedSecret `forgejo-secrets` (exists) | — | ArgoCD | re-seal; admin password reconciled on restart |
| **Forgejo SSH host key** (ed25519) | SealedSecret, mounted at `/data/ssh/ssh_host_ed25519_key` (the root image's sshd reads `/data/ssh/ssh_host_*_key`, `docker/root/etc/templates/sshd_config:13-17`; `forgejo dump` packs only `/data/gitea`, `cmd/dump.go:350-355`, so a key on the PVC alone dies with the PVC). The image generates the unused rsa/ecdsa keys itself | committed public key → workstation `known_hosts` | ArgoCD; the Cluster Bootstrap before Forgejo's first start | new key, re-seal, update the committed public key in the same commit |
| Triage runner registration token (`ftzm/triage`, repo-level; today instance-level) | agenix → VM (exists), `mode = "0400"` root: virtiofsd runs as root (microvm's `microvm-virtiofsd@` unit sets no `User=`) and so does the guest's registration service. A local reader could register a runner of its own on `ftzm/triage` and receive triage jobs with their secrets | Forgejo | — | regenerate + re-register |
| **Cache signing key** | agenix on **nuc**: `nix.settings.secret-key-files`, `services.harmonia.cache.signKeyPaths` | `trusted-public-keys` on every host (role) | the agent (nuc's own, and every host's) | add new key to the role first, keep the old trusted during overlap, then swap `secret-key-files`; old paths stay valid while the old key is trusted |
| **Claude OAuth token** (`claude setup-token`, externally issued) | agenix on **nuc**, read only by the Claude proxy (Envoy) | never in the triage VM or any Actions secret (Workflow security, Containing the agent) | nuc's NixOS config | issue a new token, re-encrypt, deploy nuc |
| `dots` runner registration token (repo-level) | agenix on nuc, root-only (`mode = "0400"`), read by the runner unit's registration (`tokenFile`). A reader could register a runner of its own on `dots` and receive `dots` jobs with every secret | Forgejo | — | regenerate + re-register |
| **Writer's nas-mirror key** (read-only) | agenix on nuc, `fleet-writer` user | public half in the nas `git` user's `authorized_keys`, restricted to `git-upload-pack`; nas host key in `known_hosts` on nuc | nas's NixOS config; the agent | rotate at will — a bad key stops the writer (no deploys) and the fix goes by workstation push |
| **`triage` bot token** | minted by OpenTofu (`forgejo_personal_access_token`), held in its state | secret of `ftzm/triage` only (the gather step's reads, the publish step's AGit pushes and comments). The bot is a **read-only** collaborator on `dots` without Actions write — it pushes no `dots` branch, opens PRs only by AGit (`refs/for/…`), which are fork PRs — and has **no access to `ftzm/triage`**: `triage.yml` checks out its own repo and uploads artifacts with the automatic token (Workflow security, "Where untrusted content runs") | the OpenTofu job creates the user, its read access on `dots`, the token and the `ftzm/triage` secret | taint the token resource, re-apply |
| **`merger` bot token** | minted by OpenTofu, held in its state | `dots` repo secret used only by `ci.yml`'s `merge-repair` merge step, the `nvd diff` comment step, `triage-verdict.yml`'s merge step and, if the automatic token cannot, `update-prs.yml`'s update step; the `merger` user has write on `dots` and is in master's merge whitelist, not its push whitelist | the OpenTofu job creates the user, its access, master's whitelists, the token and the secret | taint the token resource, re-apply |
| **ArgoCD repo credential** | SealedSecret with `argocd.argoproj.io/secret-type: repo-creds` + `sshPrivateKey` (new; GitHub needs none) | public half in the nas `git` user's `authorized_keys`, restricted to `git-upload-pack`; nas host key via `configs.ssh.extraHosts` | ArgoCD; nas's NixOS config | re-seal + new key in the nas config |
| **Push-mirror key** | generated by Forgejo for the push mirror, stored in its DB (restored with the dump) | public half in the nas `git` user's `authorized_keys` (write) | the OpenTofu job's push-mirror step creates the mirror; nas's NixOS config | recreate the mirror, replace the public half |
| **`triage-dispatch` bot token** | minted by OpenTofu, held in its state | `dots` secret `TRIAGE_DISPATCH_TOKEN`, used only by `dispatch-triage`'s dispatch step; the user has write on `ftzm/triage` (dispatch needs Actions write) and no access to `dots`, so the VM never holds a credential that can change `ftzm/triage` | the OpenTofu job creates the user, its access, the token and the secret | taint the token resource, re-apply |
| Actions secrets: `TRIAGE_DISPATCH_TOKEN`, `MERGER_TOKEN`, `RENOVATE_TOKEN` on `dots`; `TRIAGE_TOKEN` on `ftzm/triage` | the minted tokens from OpenTofu state | `forgejo_repository_action_secret`; never user- or instance-level secrets (Workflow security, rule 3) | the OpenTofu job | re-apply |
| `RENOVATE_TOKEN` itself | a PAT of a dedicated **`renovate` bot user** (repo rw, user r, issue rw), not the owner's | Renovate | the OpenTofu job creates the user and mints the PAT | taint the token resource, re-apply |
| **`monitor` bot token** | minted by OpenTofu (`read:admin`), held in its state | a Kubernetes Secret in the Forgejo namespace, read only by the runner-status CronJob (Runner monitoring) | the OpenTofu job creates the user, the token and the Secret | taint the token resource, re-apply |
| Automatic `GITHUB_TOKEN` | Forgejo | jobs | — | — |
| Cloudflare token (cert-manager), borgbase | SOPS / agenix (exist) | — | — | unchanged |

No GitHub token remains (`PAT_TOKEN` goes). A github.com read token for
Renovate's datasource rate limits is added only if Renovate gets
rate-limited.

**Forgejo configuration as OpenTofu**, with the community provider
[svalabs/terraform-provider-forgejo](https://github.com/svalabs/terraform-provider-forgejo)
— no Kubernetes-native operator for Forgejo repo settings exists. The config
declares the repos (`dots`, `ftzm/triage`), both with `private = true` —
the provider's `forgejo_repository.private` defaults to `false`
(`internal/provider/repository_resource.go:532-537`), and both repos hold
`dots` content (goal 1; `ftzm/triage`'s run logs and transcripts carry the
distilled CI log, the `dots` tree and the agent's notes), so leaving it
out would create them public and keep re-asserting that at every sync —
the bot users `renovate`,
`triage` (read on `dots`), `triage-dispatch` (write on `ftzm/triage`), `merger` and `monitor` (`forgejo_user`, `forgejo_collaborator`),
their tokens (`forgejo_personal_access_token`), master's protection
(`forgejo_branch_protection`: `enable_status_check`,
`status_check_contexts`, `block_on_outdated_branch`, merge and push
whitelists), deploy keys and repo secrets
(`forgejo_repository_action_secret`). The provider has no push-mirror
resource, so the Job's last step creates the `master` push mirror to nas by
API if absent.

**Runner monitoring** (decided 2026-10-06, after the VM runner sat offline
for ~30 hours after nuc's power loss with nothing alerting): Forgejo's
Prometheus metrics have no runner state (`modules/metrics` in v16.0.5), so
the OpenTofu job also creates a `monitor` bot user with a read-only admin
token (`read:admin`), written to a Kubernetes Secret in the Forgejo
namespace. A CronJob there (every 2 min) reads `GET /admin/actions/runners`
with it and exposes `forgejo_runner_last_online_timestamp{runner}` (and
`forgejo_runner_online{runner}`) for Prometheus; `ForgejoRunnerOffline`
fires when a registered runner has not been online for 15 minutes. The
token is declared like every other bot token, so a rebuilt Forgejo
re-mints it at the next sync.

- **Where it runs:** a PostSync hook Job in the Forgejo Application
  (`hook-delete-policy: BeforeHookCreation`), so it runs only when that
  Application syncs — a change to the config, which lives in an ordinary
  ConfigMap of the app, makes it OutOfSync — and only once its resources are
  Healthy, so Forgejo's API is up. Unrelated commits run nothing.
- **Image:** nix-built, `opentofu.withPlugins` with the provider; nothing
  downloads at run time. The provider is not in `nixpkgs-ftzmlab` (no
  Forgejo entry in its terraform-providers set), so the repo packages it
  with nixpkgs' `terraform-providers.mkProvider` (owner `svalabs`, repo
  `terraform-provider-forgejo`, pinned version, `hash` and `vendorHash`),
  exported as `packages.x86_64-linux.terraform-provider-forgejo`, its
  expression in `pkgs/terraform-provider-forgejo.nix` — outside Rule R's
  allowlist — importing nothing from `role/` or `machines/`: `nix-update`
  evaluates impurely (`nix-instantiate --eval --strict` on its `eval.nix`,
  which loads the flake by `getFlake`, `nix_update/eval.py:115-150`; flake
  code loaded that way read `$HOME` and `/etc/hostname`, where pure `nix
  eval` refused both), inside the Renovate job with `RENOVATE_TOKEN` and
  the runner's `.runner` in reach, so only owner- or Renovate-authored code
  may be on that evaluation's path; flake outputs are lazy, so it forces
  only this attribute. Renovate
  keeps it current: a regex manager in `cluster/renovate.jsonnet` on its
  `version`, datasource `github-releases`, `depName`
  `svalabs/terraform-provider-forgejo`; a `postUpgradeTasks` command
  `nix-update terraform-provider-forgejo --flake --version {{newVersion}}`,
  which rewrites the version and recomputes both `hash` and `vendorHash`
  (nix-update handles `buildGoModule`'s `vendorHash`), with
  `fileFilters` limited to the package file; `renovate.yml`'s
  `RENOVATE_ALLOWED_COMMANDS` gains
  `^nix-update terraform-provider-forgejo --flake --version [0-9.]+$` next
  to the `tk` and render entries (`renovate.yml`), and
  `nix-update` joins the cluster dev shell Renovate runs in. Forgejo is a
  supported Renovate platform with no post-upgrade-task restriction
  (renovate `lib/modules/platform/forgejo/readme.md`; `allowedCommands` is
  the current name of `allowedPostUpgradeCommands`).
- **State:** OpenTofu's `kubernetes` backend, a Secret in the Forgejo
  namespace; it also holds the minted tokens. Lost state is rebuilt with
  `tofu import` or by re-minting the tokens.
- **Credentials:** the sealed admin credentials from `forgejo-secrets`. The
  one externally issued secret, the Claude OAuth token, is not Forgejo's:
  it lives in agenix on nuc for the Claude proxy (Secrets table).
- Drift from edits in Forgejo's UI is corrected at the next sync of the app.
- To confirm on first apply: the provider against Forgejo 16.0.5.

**Roots of trust, and what a from-scratch rebuild needs**: the agenix master
key (`personal` in `secrets/secrets.nix`) and the **sealed-secrets controller
private keys**. The first is the owner's ssh key. The second is **not backed
up anywhere in this repo** — without it a rebuilt cluster cannot decrypt
`forgejo-secrets` or anything else sealed. The controller keeps every key it
has made and `kubeseal` seals with the newest, so the backup must hold all of
them and the set must stop growing: `lab.jsonnet`'s `sealedSecrets` passes
the chart `keyrenewperiod: '0'` (the controller default is a new key every 30
days, `cmd/controller/main.go:24`; the chart omits the flag when the value is
empty), deployed through ArgoCD. Once that is synced, the backup (`kubectl get
secret -n sealed-secrets -l sealedsecrets.bitnami.com/sealed-secrets-key -o
yaml`, every key) goes into agenix. Both are the first action of step 1, in
that order, so no key appears between them. Everything else regenerates:
Forgejo's DB holds the Actions secrets encrypted with the pinned
`SECRET_KEY`, so a dump restore brings them back, and the OpenTofu job
re-asserts the rest.

## nuc Down

nuc is the deploy plane. These procedures are the pre-migration manual paths,
kept first-class.

**Short term (bad commit, hardware hiccup).** Nothing deploys anywhere; hosts
keep running; CI, Renovate and flake bumps pause; workstation pushes to git
queue locally. nuc cannot heal itself through the agent (the fix would have
to be published by the writer on nuc), so its recovery is always manual.
**Step 1 on any host changed by hand: `systemctl stop fleet-agent.timer`** —
the manifest still names the bad commit, and wherever nuc's writer, nginx
and harmonia still work the agent would switch the host back to it within
two minutes. Then: `nixos-rebuild --rollback` over ssh (previous generation on disk), the
bootloader menu at the console if ssh is gone, or a push from the
workstation — `nixos-rebuild switch --flake .#nuc --target-host admin@nuc` —
which evaluates and builds nuc's closure on saoiste from cache.nixos.org plus
the glue, exactly as `make pi` does today. Anything else that must change
during the window goes the same way, to that host, timer stopped first.
Last step: push the fix to master (nas, and Forgejo when it is up), wait
for the writer to publish it, then `systemctl start fleet-agent.timer` on
each host that was stopped; each agent compares the manifest's path with
what is running and converges.

**Long term (weeks).** The homelab is down: the cluster, Forgejo, the cache.
The workstation clone is the source of truth for the interim; every deploy is
a workstation push. A fresh nuc's agent has no manifest to fetch until nuc
itself publishes one, so rebuilding nuc is ordered:

1. Install nuc from the workstation (push).
2. Bootstrap the cluster and restore Forgejo with `cluster/scripts/bootstrap`
   (Cluster Bootstrap): ArgoCD reads the nas mirror; Forgejo's `master`
   comes from nas.
3. Register the runner.
4. The writer's timer picks up nas `master`: it builds every host on nuc (the store is empty; this
   is the one cold build), then publishes a manifest.
5. Only now do agent deploys resume, nuc's own included.

**Keeping a manual switch (any time nuc is up).** The agent reverts a
manual `nixos-rebuild` on its next tick. When a host must stay on something
master does not yet name — Forgejo down and an urgent fix on nas, say —
`systemctl stop fleet-agent.timer` on that host first, push, and
`systemctl start fleet-agent.timer` once master has the fix. A stopped timer
shows as a stale `fleet_last_success_timestamp`, which is the reminder.

**Laptops meanwhile (either case, or simply away from home).** Each laptop
keeps a clone of `dots` and can `nixos-rebuild switch --flake .#<host>` itself
— local eval and build, cache.nixos.org for binaries, no infrastructure.

**Self-tests.** Both manual paths run weekly in a form that changes nothing,
each writing `fleet_manual_path_last_success_timestamp{path,host}` to its
node_exporter textfile dir; a `Fleet*` rule alerts on failure, or on a
stamp older than 8 days that stays so for 2 h (`for: 2h`). Neither host is
always on (eachtrai's comin target was up 29% of the 30 days to
2026-10-05), and an off host's series is not scraped, so the rule only sees
a host while it is up: the timers are `Persistent = true` and catch up a
missed run after boot, and the 2 h covers that run. A host that stays off
is never alerted on — it cannot self-test either:

- **Laptop self-rebuild:** a weekly timer on each laptop runs `git fetch` in
  its clone and `nixos-rebuild build --flake <clone>#<host>` — clone,
  evaluation and substitution, no switch.
- **Workstation push:** a weekly timer on saoiste runs `nixos-rebuild
  dry-activate --flake .#<host> --target-host …` for each lab host — ssh
  access, sudo and the build, nothing activated.

## Forgejo Upgrades

Forgejo image bumps automerge like everything else. The upgrade follows
Forgejo's documented procedure — backup, new version, `forgejo doctor check
--all` — and a failure rolls itself back and locks further automatic
upgrades until the owner lifts the lock. Downgrading the image alone is
impossible: an older Forgejo refuses a newer schema
(`models/forgejo_migrations/migrate.go:185`), so rollback always means the
pre-upgrade snapshot plus the old image; Forgejo's guide: "Restoring the
backup done before the upgrade is easy and does not lose any information".

**Requires Forgejo to be its own ArgoCD Application** — it is since
2026-10-05, generated by the `services` ApplicationSet of
`ARGOCD_APPLICATIONS_PLAN.md`. Hooks, sync failure and the lock are all
per-Application: inside the former single `lab` app, a failing pre-flight would stop
every cluster sync ("If any of them fails the whole sync process will stop"),
any unrelated failure would fire the Forgejo rollback, and the lock would
pause the whole cluster. The ApplicationSet carries
`ignoreApplicationDifferences: [{jsonPointers:
[/spec/syncPolicy/automated/enabled]}]` — ArgoCD's documented way to let one
generated Application's auto-sync be switched off without the controller
reverting it, narrowed from the docs' `/spec/syncPolicy` so other syncPolicy
fields still follow the template (`ARGOCD_APPLICATIONS_PLAN.md` → Target).

The phases, all in `cluster/lib/backup.libsonnet` next to the existing gate:

**Attempt marker.** `/backup/.upgrade-attempt` on the backup PV —
`<from> <to> <snapshot path> <stage>` — exists only while an upgrade is in
flight: the gate writes it before touching anything, the PostSync check
deletes it as its last step after passing, SyncFail deletes it once it has
handled the failure. SyncFail acts only on the marker, never on which
snapshots exist: snapshots stay on the PV after a successful upgrade, so a
rule of "a `<from>-to-<to>` snapshot exists and Forgejo is not answering
`<from>`" would, after a successful upgrade to `X`, restore the
`<prev>-to-X` snapshot on any later Forgejo sync failure while master still
names `X` — a downgrade losing everything written since.

1. **Gate — snapshot and pre-flight** (extends `forgejoDumpGate`), a
   `Sync` hook in wave `-1`, not PreSync (`ARGOCD_APPLICATIONS_PLAN.md` →
   Hooks and waves): PreSync runs before any Sync-phase resource exists, so
   on a fresh cluster its `forgejo-data` and `forgejo-backup` PVCs would not
   exist and the Job's pod would never schedule. Those PVCs and the
   `forgejo-backup-forgejo` PV sit in wave `-1` with it; everything else,
   the Deployment included, in wave `0`, which gitops-engine does not start
   while a hook of wave `-1` runs or after one failed
   (`pkg/sync/sync_context.go:494-500`, `:540-543`). In order:
   1. No `/data/gitea/gitea.db` on the volume → fresh install, exit 0.
      (Already in place: the ArgoCD cut-over made the gate a wave-`-1` Sync
      hook with this check, `lib/backup.libsonnet`.)
   2. The running instance's version (`/api/v1/version`, as today) equals
      the target image's tag → exit 0: nothing to upgrade, lock or not.
   3. Upgrade pending and the Forgejo Application has
      `spec.syncPolicy.automated.enabled: false` (the lock) → exit 1
      ("upgrade `<from>`→`<to>` locked, see runbook") without touching
      anything.
   4. Otherwise: write the marker with `stage=preflight`; scale Forgejo to
      0 and take the snapshot
      `/backup/forgejo-preupgrade-<from>-to-<to>-<ts>.tar.gz` of `/data`
      with it stopped — Forgejo's guide asks for a synchronized
      point-in-time snapshot, and today's gate tars `/data` under a live
      SQLite; then the **pre-flight**: extract the snapshot into an
      `emptyDir` mounted at `/data` in a container of the *new* image, run
      `forgejo migrate` and `forgejo doctor check --all` there (live data
      untouched). Failure anywhere → scale Forgejo back to 1 on the old
      image, exit 1 with the reason; a failed gate stops the sync before any
      wave-`0` manifest is applied (only the wave-`-1` PVC/PV applies
      precede it, no-ops on a live cluster), so the old version keeps
      serving. Success → set the marker's `stage=rollout`, exit 0.

   Its ServiceAccount may read the Forgejo Application and scale the
   `forgejo` Deployment.
2. **Sync, then PostSync — the real upgrade and its check.** The Deployment
   rolls; the `forgejo migrate` initContainer migrates live data. The
   Deployment carries an explicit `progressDeadlineSeconds`: with hooks
   present ArgoCD waits on each resource's health, and a Degraded resource
   fails the sync (gitops-engine `pkg/sync/sync_context.go:486-487`). Once
   everything is Healthy, a PostSync Job mounting `forgejo-data` (RWO
   permits a second pod on the same node) runs `forgejo doctor check --all`
   and one API request; failure fails the sync. On success it deletes the
   marker, its last step.
3. **SyncFail — roll back and lock, only for a failed upgrade.** Runs when
   the sync fails ("Executes when the sync operation fails"; gitops-engine
   runs SyncFail tasks for a failed task of any phase, `setOperationFailed`,
   `sync_context.go:745-754`):
   - No marker → exit 0. The failure was not an upgrade: no rollback, no
     lock, and ArgoCD's default retries (below) proceed.
   - `stage=preflight` → ensure Forgejo is at 1 replica on `<from>` (covers
     a gate killed mid-way), lock, delete the marker, exit 1 naming the
     failed phase and versions.
   - `stage=rollout` → if Forgejo is not answering `<from>`: scale to 0,
     restore *the marker's* snapshot into `forgejo-data`, set the
     Deployment's image to `<from>`, scale to 1, wait for `/api/v1/version`
     to report `<from>`, run `forgejo doctor check --all`. Then lock, delete
     the marker, exit 1 naming the failed phase and versions.

   Lock = `spec.syncPolicy.automated.enabled: false` on the Forgejo
   Application (ArgoCD's documented switch: the controller "will skip
   automated sync even if prune, self-heal and allowEmpty are set"). Its
   ServiceAccount may patch only the `forgejo` Deployment and the Forgejo
   Application.

**Why only a failed upgrade locks.** The lock exists because "Automatic sync
will not reattempt a sync if the previous sync attempt against the same
commit-SHA and parameters had failed", yet any later commit is a new SHA and
would retry the upgrade — a concern only after a failed upgrade. Any other
Forgejo sync failure (an API-server blip on single-node k3s, say) gets
ArgoCD's default retry: automated syncs carry `Retry: {Limit: 5}` unless the
app sets `syncPolicy.retry` (`controller/appcontroller.go:2369-2373`),
backoff 5 s ×2 capped at 3 m (`application_defaults.go:6-8`).

**Retries after a failed upgrade are harmless by construction.** The retry
decision checks only `RetryCount < Limit` (`appcontroller.go:1620-1631`),
not the lock, and every retry recreates the fixed-name hook Jobs
(`BeforeHookCreation`, `sync_context.go:1449-1470`). SyncFail completes
before the operation is marked Failed (`sync_context.go:745-757`), so the
lock is set before the first retry: each retry's gate stops at step 3 and
its SyncFail finds no marker — about 2.5 min of no-op attempts, then the
sync reports Failed. Without step 3, a retry after a rollback would see
`<from>` ≠ target and run the whole upgrade again, up to 5 times.

| Case | Outcome |
|---|---|
| Transient failure on a non-upgrade sync | no marker → SyncFail no-op → retry recovers silently |
| Pre-flight fails | lock; retries no-op |
| Rollout or PostSync check fails | restore from the marker's snapshot, lock; retries no-op |
| Transient failure during an upgrade's wave `0` | treated as a failed upgrade: restore, lock (a sync cannot tell a blip from a bad upgrade; upgrades are rare) |
| Any failure after a successful upgrade | no marker → no restore |
| Lock lifted (Restore Runbook → Forgejo upgrade lock) | gate step 3 passes → full procedure |

**Alerting:** `ForgejoUpgradeLocked` fires while the Forgejo Application has
automated sync disabled; the failed sync already raises the ArgoCD sync
alerts. The Job logs name the failing phase and versions. The lock is lifted
per Restore Runbook → Forgejo upgrade lock.

## Cluster Bootstrap

ArgoCD's source is the nas mirror, which does not depend on the cluster, so
a fresh or wiped cluster bootstraps the ordinary way: install ArgoCD, point
it at the mirror, let it deploy everything — Forgejo included — then restore
Forgejo's data. Nothing in the repo does this today: `machines/nuc/k3s.nix`
installs only k3s, no `.nix` file references ArgoCD or sealed-secrets, and
`cluster/README.md` has no procedure.

**`cluster/scripts/bootstrap`**, committed, run from the workstation clone
against a k3s that the nuc NixOS install brought up. It is the `CLAUDE.md`
one-time-bootstrap exception; every `kubectl apply` it makes is of committed
files from `cluster/manifests`, which ArgoCD adopts unchanged. In order:

1. **Sealing keys before the controller.** Decrypt the sealing keys from
   agenix (Secrets) and apply them as the labelled key Secrets, then the
   sealed-secrets CRD and controller. Reversed, the controller generates a
   fresh key on first start and no existing SealedSecret decrypts.
2. **ArgoCD**: its repo credential, then a server-side apply of
   `cluster/manifests/argocd/` — the install, the self-managing `argocd`
   Application, the operator Applications in sync waves and the
   ApplicationSet (`ARGOCD_APPLICATIONS_PLAN.md` → Target, step 3), so
   operators and their webhooks are Healthy before any service app exists. ArgoCD reads the nas mirror and syncs every service; Forgejo
   comes up empty (its upgrade gate, a wave-`-1` Sync hook with its PVCs in
   the same wave, sees no database and passes; its SSH host key comes from
   its SealedSecret). The immich, miniflux and pinepods gates pass the same
   way: no cnpg `Cluster` yet (`ARGOCD_APPLICATIONS_PLAN.md` → Hooks and
   waves). An
   empty Forgejo is harmless: ArgoCD does not read it, and it has no push
   mirror, runners or tokens configured.
3. **Restore Runbook steps 0–10** against it.

The script stops on any failure; key Secrets present before the controller
Deployment is the one ordering it asserts.

**Rehearsal, once, before step 3 of the Plan**: run the script end to end
against a scratch single-node k3s NixOS VM on the workstation, restoring the
latest nightly, and finish with runbook step 10's checks against it. Time
it; that number goes here.

## Restore Runbook

For: nuc's disk lost, `forgejo-data` PVC gone, SQLite corrupted by a migration.
Not for: Forgejo merely down (see S1/S3 — fix the cause, the data is intact).

What exists:

- **Nightly** `forgejo dump --type tar` (CronJob `forgejo-dump`, 02:00) →
  `/pool-1/k8s/forgejo-backup/forgejo-<ts>.tar` on nas, 7-day retention, in
  the borgbase job. Contents: `app.ini`, `data/` (the raw `gitea.db` +
  `-wal`/`-shm` copied live, `conf`, `jwt`, `actions_*`, `avatars`, `queues`,
  `indexers`, …), `forgejo-db.sql`, `repos/ftzm/`.
- **Pre-upgrade raw snapshots** by the ArgoCD upgrade-gate hook job
  (`forgejo-preupgrade-<from>-to-<to>-<ts>.tar.gz`), taken because `forgejo
  dump` is not forward-compatible across schema versions.
- The Forgejo upgrade guide warns the SQL dump "has serious long standing open
  bugs that may introduce problems when re-injecting". **Restore the raw
  `data/` copy, not `forgejo-db.sql`.** The SQL is the fallback if the raw DB
  fails its integrity check.
- Full copies of the git content independent of the dump: the nas mirror
  (`master`, never behind Forgejo's last push) and the workstation clone
  (PR branches). The manifest is not in git; the writer
  regenerates it from master.

Why order matters: a restored dump is hours old. nas refuses to go
backwards, so deploys are safe regardless (Decisions → nas `master`); but a
Forgejo running on stale refs would have its mirror refused until fixed, and
the runner and Renovate would act on stale branches and PRs. So refs are made
current on disk before Forgejo first starts, and automated sync stays paused
until the end.

Prerequisites: the agenix master key (owner's ssh key) and the
sealed-secrets controller key (in agenix, Secrets). `forgejo-secrets` pins
`SECRET_KEY`, so the restored DB's encrypted Actions secrets and tokens
remain readable.

Steps (the `kubectl` here is the one-time bootstrap exception):

0. **Pause automated sync** on the Forgejo Application:
   `spec.syncPolicy.automated.enabled: false` (the ApplicationSet ignores
   `/spec/syncPolicy/automated/enabled`, Forgejo Upgrades) (ArgoCD's documented switch: the controller "will skip automated
   sync even if prune, self-heal and allowEmpty are set"). Without it
   `selfHeal` reverts the scale and env changes below.
1. **Pick the dump.** Latest nightly, or the newest pre-upgrade snapshot if
   the failure was an upgrade. Check it before trusting it:
   `sqlite3 data/gitea.db "PRAGMA integrity_check"` on the extracted copy.
2. **Stop Forgejo** so nothing serves: `kubectl scale deploy/forgejo -n forgejo
   --replicas=0` (strategy is `Recreate`; the PVC is RWO). If the PVC is
   gone, a manual sync of the Forgejo Application recreates it empty; if the
   cluster is gone, `cluster/scripts/bootstrap` has done that (Cluster
   Bootstrap).
3. **Restore into the PVC with a Job** that mounts `forgejo-data` at `/data`
   and `forgejo-backup` at `/backup`, as uid 1000 (`su-exec git`): extract
   the dump's `data/` over `/data/gitea/` and `repos/ftzm/` into
   `/data/git/repositories/ftzm/`; `chown -R 1000:1000`.
4. **Bring the refs current on disk, before anything runs** — before
   Forgejo's push mirror can push to nas: `master` from the nas mirror (the
   most advanced copy by construction), open PR branches from the
   workstation clone where it has them, fetched straight into the bare repo
   (`git -C /data/git/repositories/ftzm/dots.git fetch <src> '+refs/heads/*:refs/heads/*'`).
5. **Regenerate what depends on paths and keys**, still in the Job:
   `forgejo admin regenerate hooks` and `forgejo admin regenerate keys`
   (authorized_keys for deploy keys), with `--config /data/gitea/conf/app.ini`.
6. **Start Forgejo quarantined**: `kubectl scale … --replicas=1` with
   `FORGEJO__actions__ENABLED=false` for this boot and the ssh NodePort /
   IngressRoute still absent, so the runner, Renovate and humans cannot act
   on it yet. The `bootstrap-admin` init container reconciles the admin
   account from the sealed secret; `environment-to-ini` re-bakes `app.ini`.
7. **Replay through Forgejo — the DB is still the dump's.** Step 4 fixed the
   refs on disk, but Forgejo's SQLite still holds the dump's `branch` table,
   default branch, PR states (PRs merged after the dump are still "open",
   heads pointing at now-ancestor commits), queued auto-merges, and Actions
   tasks left "running". `git ls-remote` cannot see any of it; Renovate and
   the runner would act on it (rebasing already-merged PRs, re-dispatching
   dead tasks). So, over a `kubectl port-forward` to the pod (bootstrap
   exception): push `master` (from nas) and every open PR branch from the
   workstation clone, which drives Forgejo's own push path — branch table synced, PRs
   whose heads master now contains flip to merged (confirmed by the
   rehearsal); then `forgejo doctor check --all --fix`; then cancel any
   Actions run still "running" via
   `POST /repos/{o}/{r}/actions/runs/{id}/cancel`.
8. **Forgejo configuration** — deploy keys, repo secrets, branch
   protections, bot users and the push mirror are re-asserted by the OpenTofu
   PostSync Job when step 9 syncs the Forgejo Application, not trusted from
   the dump. Runner
   registrations survive in the DB; if the runner shows offline, re-register
   with a fresh token.
9. **Expose and resume**: set `spec.syncPolicy.automated.enabled: true` on
   the Forgejo Application and sync it — Actions back on, ssh NodePort and
   IngressRoute back.
10. **Verify**, in this order: `git ls-remote` on Forgejo's `master` matches
    the nas mirror's **and** `GET /repos/ftzm/dots/branches/master` returns
    the same sha, with no `last_error` on the push mirror; `GET /pulls?state=open` lists only PRs that are really open;
    `GET /actions/runs` shows nothing "running"; the Forgejo Application synced to
    the expected sha, no prunes pending; the writer publishes a manifest for
    that sha and one host's `fleet_deployed_commit` matches; a throwaway PR
    triggers CI.

### Forgejo upgrade lock

`ForgejoUpgradeLocked` means an automatic Forgejo upgrade failed, rolled back
(Forgejo Upgrades) and switched off automated sync for the Forgejo
Application. Forgejo is running the previous version on its pre-upgrade data;
master still names the new image, so the Application shows OutOfSync.
Nothing else in the cluster is affected.

1. Read the failed sync and the SyncFail Job's log: the phase (pre-flight,
   rollout, PostSync check) and the versions.
2. Decide: wait for a fixed release, or fix the cause (config, release
   notes). Rehearse the upgrade on the snapshot if in doubt — the pre-flight
   container is the harness.
3. Lift the lock: a manual sync of the Forgejo Application with automated
   sync re-enabled (`spec.syncPolicy.automated.enabled: true`). The sync runs
   the full upgrade procedure again, snapshot and rollback included.

**Rehearsal, once, before step 3 of the Plan**: restore the latest nightly
into a scratch namespace (`forgejo-restore-test`: copy of the Deployment + a
fresh PVC, no Service/IngressRoute, Actions off) and walk steps 1–7 and the
API checks of step 10 — the replay is the part `ls-remote` cannot validate,
so the rehearsal must include a PR that was merged after the dump and confirm
it shows as merged afterwards. Delete the namespace after. Time it; that
number goes here.

---

## Verified vs. Unverified

**Verified** (source cited where used): Forgejo `16.0.5`; every ArgoCD
Application `prune`/`selfHeal`/no `allowEmpty`; `/dev/kvm` and the `kvm`
system feature on nuc; forgejo-runner `13.2.0` in `nixpkgs-ftzmlab`; `forgejo-data` PVC class and lack of prune
guard; every `file:line` in the hurdles table; Forgejo Actions event set
(`workflow_run` absent from the source), first-existing workflow directory
only, `uses:` resolution and `DEFAULT_ACTIONS_URL` default, contexts,
`concurrency` semantics, artifact requirements, Renovate's forgejo platform,
every API endpoint named (live swagger); laptops scraped over Tailscale
(`lab.jsonnet:47-52`); leigheas and eachtrai share a wg `publicKey`
(`role/network.nix:12,51`); the pi has no wg, no tailscale and no
node-exporter; the node-exporter textfile dir; the writer's host/system
evaluation on the real flake. Workflow security against Forgejo `v16.0.5`
and forgejo-runner `v13.2.0` source (every `file:line` in Workflow security
and Design): workflow source commit per event, approval and secret rules for
fork/AGit PRs, `pull_request_target` from the base without approval, status
API permission and context matching, automatic-token access (write on
its own repo for non-fork jobs, read-only for fork PRs), the automatic token
in every step's environment, `permissions:` ignored, a new status starting
the auto-merge check, `issue_comment` running the default branch's workflow,
`head_commit_id` on merge, the merging user as pusher, protected-file checks
on push, AGit permission, fork treatment and head repo, the repo- and
instance-level runner registration APIs,
`one-job` and daemon-mode refusal of single-use runners, the runner's
container defaults; nuc's listening sockets (`ss -tlnp`), its resolver (`100.100.100.100`), its netfilter backend (`nf_tables` via `nft_compat`, no `ip_tables`) and the nixpkgs `nftables` module's flush behaviour; the Claude Code sandbox's default read policy (docs);
`render-lab` and `test-rules` offline (`unshare -rn`, output identical to the
committed manifests); ArgoCD sync semantics the upgrade design uses — tasks
run one (phase, wave) at a time, a running hook holds back the next wave and
a failed task fails the sync before later waves
(gitops-engine `pkg/sync/sync_context.go:494-500`, `:540-543`, `:564-574`),
SyncFail hooks run on any failed task, whatever its phase (`:540-543` →
`setOperationFailed`, `:745-754`), a Pending PVC is `Progressing`
(`pkg/health/health_pvc.go:33-34`), PostSync waits for Healthy, a Degraded resource
fails a multi-step sync (gitops-engine `pkg/sync/sync_context.go:486-487`),
no automatic retry of a failed sync for the same SHA,
`spec.syncPolicy.automated.enabled`, `ignoreApplicationDifferences` (ArgoCD
docs; the field exists in this cluster's `v3.5.3` CRD).

**Tested in VMs and scratch harnesses:**

| # | What | Result |
|---|---|---|
| T3 | `nix build <path>` in an empty store substitutes the output from an HTTP cache | ✓ — this is the agent's fetch |
| T5 | harmonia serves signed outputs | ✓ on 2.1.0 (workstation) and on 3.1.0 (NixOS VM test); the nuc deployment itself remains |
| T6 | closure sizes, dedup totals, what builds from source | ✓ (Sizing) |
| T12 | nuc's daemon as the boundary for an untrusted client | ✓ as trusted users; the refusal of unsigned `nix copy` for an untrusted user is nix's documented `require-sigs`, still to check with the real `gitea-runner` user |
| T13 | agent against harmonia 3.1.0 + nginx `/fleet/` in a NixOS VM test on the `nixpkgs-ftzmlab` tree | ✓ for signature check, substituting only the changed paths (7 paths, 27 MiB), `nix-env --set` + `switch`, metrics, no-op run, tampered manifest refused. NixOS test VMs need `system.switch.enable` + a real bootloader for `switch-to-configuration`. The harness's agent reads an ssh-signed manifest file, not the store-path manifest |
| T14 | writer against a scratch bare repo with two fake hosts | ✓ for the no-op on unchanged master, README-only commit publishing nothing, one-host change, revert commit republishing the older path. The harness's writer used a `seq` state file, ssh signing and hand-registered roots, not the two profiles |
| T15 | pure flake evaluation reading an absolute path (nix 2.34.8, workstation and nuc) | an input `path:<abs file>` with no lock entry is locked and its content returned by `nix eval` (pure, `--no-write-lock-file`). With `--no-update-lock-file` it is refused, as are a lock entry without `narHash`, an unlocked `fetchTree` and `builtins.path`; a lock entry with a wrong `narHash` fails but the error prints the file's real hash |
| T17 | network reach of evaluation and builds (nix 2.34.8) | a hashed `builtins.fetchurl` under a fresh name succeeds in pure evaluation with network and fails with "Could not resolve host" when only the evaluating client has none (so the fetch runs in the client); a fresh fixed-output derivation (`builtin:fetchurl`) built with `sandbox = true` and substituters off fetched `https://cache.nixos.org/nix-cache-info`; every host toplevel, `homeConfigurations.ftzm` and the cluster dev shell evaluate with no network and `allow-import-from-derivation = false` to their usual `drvPath`s; enumerating nas's closure takes 0.9 s |

The reference scripts are `scratchpad/t13/fleet-agent.sh` and
`scratchpad/t14/fleet-write.sh` in a session scratchpad, not in the repo.

**Verified in step 1 (2026-10-06):** the automatic token on a protected
branch and on PR endpoints (hurdles table); Actions on a pull-mirror repo
(Plumbing); a repo-scoped runner is invisible to other repos — a runner
registered to one private repo took that repo's job at once while a job in a
second repo with the same `runs-on` stayed `waiting`, and the runner was
handed only the first repo's task; `render-lab`/`test-rules` as sandboxed
derivations (`cluster/flake.nix` checks, CI builds them); the store-path
manifest and the two profiles end to end (`checks.x86_64-linux.fleet-deploy`
passes, and the writer's first manifest on nuc names exactly the systems
comin deployed on saoiste, nuc and nas).

**Unverified, to confirm on first use:** the
runner's host-mode label syntax; how Forgejo's ssh push mirror verifies the
remote's host key, and that a push mirror created by the OpenTofu job's API step pushes
on commit (step 3); the Forgejo
upgrade hooks end to end (step 3's deliberate failures); that the
`microvm-egress` table filters SLIRP connections (step 4's negative checks);
whether Claude Code's Linux sandbox gets its own PID namespace (undocumented;
the `denyRead` rule and step 4's `/proc` check cover it either way); the untrusted-client refusal with the real
`gitea-runner` user (step 2); that the `triage` bot, read-only on `dots`,
may read `dots`' Actions job logs and comment on its PRs; `dispatch-triage`'s
`workflow_dispatch` into `ftzm/triage` with inputs; that a PR comment fires
`issue_comment` workflows; Claude Code 2.1.x through the Claude proxy
with only the proxy holding the OAuth token (`ANTHROPIC_BASE_URL`, Envoy's
`credential_injector` overwriting `Authorization`, in the `nixpkgs-ftzmlab`
Envoy); Claude Code's sandbox under the host-mode runner in the VM guest,
with the managed settings of Containing the agent; the agent's `nix` on a
workdir store inside the sandbox, evaluating every host `--offline` from
what `nix flake archive` put there (import-from-derivation anywhere in the
tree would need builds); the VM's 20 GiB volume against that store;
that `nix-update` rewrites a `terraform-providers.mkProvider` call's version and both hashes (first provider bump);
`pull_request_target` firing for an AGit PR on the live instance;
Renovate automerging its PR after a repair merged into its branch (the
armed auto-merge surviving another user's push); that nas's `receive.denyNonFastForwards` refusal of a mirror push surfaces
in the push mirror's `last_error`, and that `branch_filter: master` pushes
only `master`; that a single `nix build` holds temporary roots for its
finished sub-builds until it exits under `min-free` pressure (if not, one
`--out-link` for the linkFarm covers the window).

---

## Follow-ups

Found while checking step 1; not blocking any step.

### eachtrai runs an April system although comin reports its switches succeeded

Facts (2026-10-05):

- Prometheus's `node-exporter` target `http://100.64.0.7:9002/metrics` is
  down (`connect: connection refused`; `avg_over_time(up[30d])` 0), and no
  alert covered it. eachtrai has no `prometheus-node-exporter` unit: it was
  added to eachtrai on 2026-09-12 (`bf86b68d`), after the system it runs.
- `/run/current-system` and `/run/booted-system` are both
  `1d9vdr17…-nixos-system-eachtrai-26.05.20260411.1304392`;
  `/nix/var/nix/profiles/system` is `7nr313q8…-26.11.20260926.e158d9e`;
  master builds `mqvq1h8w…-26.11.20261001.c59305b`.
- `comin status` reports `Deployment succeeded`, `Operation switch`,
  outpath `mqvq1h8w…`, profile `/nix/var/nix/profiles/system-profiles/comin-64-link`.
  So comin records switches as successful that did not activate.

Unverified: whether each `switch` since the 26.05 → 26.11 move exits 100
(the new systemd's interface version differs from the running PID 1, the
plan's deferred switch, Binary Cache → Design → Agent) and comin counts that
as success; and which entry the bootloader boots next. To check, on
eachtrai: `who -b`; `journalctl -u comin --since -2d | grep -iE
'exit|status 100|reboot|switch-to-conf'`; `ls -l /nix/var/nix/profiles/
/nix/var/nix/profiles/system-profiles/`; `bootctl status`.

Then: reboot eachtrai into the current generation, confirm node_exporter
answers on `:9002` and the laptop self-test reports. Under `fleet-agent`
(step 2) this case is handled — a deferred switch sets
`nixos_reboot_required{reason="deferred"}` — but that metric also travels
through node_exporter, so it needs this fixed. The silent dead target is a
gap of its own: laptops are excluded from the reachability alerts
(`alwaysOn: false`), so nothing notices a laptop scrape that never
succeeds while the host is otherwise up (its comin target answered); an
alert on `up{job="node-exporter"} == 0` while that host's comin target is
up would.
