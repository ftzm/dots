# Logging & Alerting Overhaul Plan

## Context

Two incidents in August 2026 exposed that this infrastructure's failure modes are largely invisible:

1. **ArgoCD sync deadlock (9 days):** a broken forgejo PreSync gate blocked every ArgoCD sync for 9 days. Nothing alerted; it was found only by chasing a user-reported missing file in Navidrome.
2. **OpenWrt node clock:** `NodeClockNotSynchronising` fired for months on the friendlywrt node (busybox ntpd on OpenWrt 24.10 runs with `-N` and its time-setting hotplug path is dead; kernel `STA_UNSYNC` never clears because `time_maxerror` pins at 16s). Root-caused and fixed with chrony, but again: the alert existed only because kube-prometheus-stack happened to ship it.

The audit below is the state of the observability stack as of 2026-08-30, plus a phased plan to make log-based visibility and alerting trustworthy.

---

## Current State (audit findings)

### Pipeline

```
pod logs      ─┐
host journal  ─┼─► alloy (daemonset, v1.19.2) ─► loki 3.6.11 (filesystem, 720h retention)
               │        └─► tempo (OTLP traces)
k8s events    ─┘   (not collected)
friendlywrt syslog  (not collected)
```

- **Pod logs:** all namespaces/containers. Ingest pipeline has a single `stage.json` (extracts `level`/`msg`/`message`); a `level` label is promoted only for JSON logs matching a regex.
- **Host journal:** only `nuc` reaches Loki. `nas` *is* configured to ship it (promtail, `machines/nas/default.nix:221-262`, same label set, targeting the cluster Loki) but has delivered nothing for 32 days — see `INCIDENT-2026-08-wireguard-transport.md`. Labeled `unit`, `level` (systemd priority), `host`, `syslog_identifier`.
- **OTLP traces → Tempo**, OTLP logs not used.
- **Loki:** filesystem storage, 720h (30d) retention, memcached chunk cache. The ruler is **not usable as shipped**: the rendered config (`cluster/manifests/lab/loki-configmap.yaml:75-79`) sets only `storage.type: local` plus a WAL dir — no `alertmanager_url`, no `ruler.storage.local.directory`. Rule *delivery* does work (a k8s-sidecar watches ConfigMaps labeled `loki_rule` into `/rules`, `loki-statefulset.yaml:93-110`), but a rule that fires notifies nobody. Wiring this is unbuilt work, not existing capability.
- **Alerting:** kube-prometheus-stack metric rules (node/pod/deployment/job level) + newly added ArgoCD rules (verified scraped: controller ServiceMonitor works, `argocd_app_reconcile_count` present in Prometheus at a regular ~22/hour cadence) → Alertmanager → ntfy. **No log-based rules, no Grafana-managed rules, no k8s events.**

### Log volume (48h window, ~500k lines)

| Source | Lines/48h | Nature |
|---|---|---|
| storage-test | 232k (46%) | leftover NFS-write test pod (`cat`s an ever-growing file each restart) |
| traefik | 147k | access log, every request |
| blocky | 112k | queryLog, every DNS query |
| kube-system | 13.6k | coredns |
| monitoring | 5.7k | loki/prometheus/alloy |
| argocd | 3.3k | |
| everything else | ~5k | |

98% of ingested lines are query logs and a test app.

### Format inventory (parseability)

| Format | Services | `\| level = "error"` works? |
|---|---|---|
| logfmt (`level=info msg=...`) | argocd, loki, alloy, prometheus | ✅ detected |
| JSON | traefik, cnpg, miniflux, immich-postgres | ✅ |
| `[INFO]`/`[WARNING]` bracket | blocky, media/arr, vaultwarden, coredns | ❌ |
| `INFO` prefix | ntfy | ❌ |
| `[I]` (gitea) | forgejo | ❌ |
| Redis-style | immich valkey | ❌ |
| journal (nuc) | k3s, comin, jellyfin, ... | ✅ (explicit label) |

### Known parsing gaps (verified)

- **argocd logs `level=warning` (730 lines/48h) but Loki's detector labels every line `info`** — `| level = "warn"` returns 0. Silent false-negative.
- **blocky `response_code=NOERROR`** and **JSON `"error":null` fields** false-positive naive `"error"` substring searches.
- **Loki logs its own queries at info**, polluting the monitoring stream (any regex search for "error" hits your own queries).
- A `| level = "x"` filter on an undetected format returns nothing **without error** — an alert written today against blocky/media/ntfy would silently never fire.

### Missing sources

- friendlywrt (OpenWrt) host syslog — only its pod logs are collected. The clock bug above was invisible in the stack.
- Kubernetes events (OOMKill, FailedScheduling, evictions) — collected nowhere.

---

## Related incident: WireGuard transport rot

The `nas` journal gap was a broken-transport outage (dead WireGuard tunnel, 32
days), which also froze `saoiste`'s PID 1 and silently killed its deploys for 26
days. Postmortem and remediation backlog: `INCIDENT-2026-08-wireguard-transport.md`.
Its lessons shape this plan: no service path should traverse infrastructure that
can rot unnoticed, and absence-shaped failures need absence-shaped alerts.

---

## Target State

One sentence: **normalize every stream into one schema at ingest (level + message + bounded labels), route by value (drop or short-retain what metrics cover), then alert uniformly through Loki ruler + PrometheusRule → Alertmanager → ntfy.**

```
any format ─► alloy (one pipeline, per-format matchers) ─► level label + message structured metadata
                                                              │
friendlywrt syslog ─┘   k8s events ─┘                          ▼
                                                       Loki ruler rules  ─► Alertmanager ─► ntfy
                                                       PrometheusRule     ─► Alertmanager ─► ntfy
```

---

## Code structure (jsonnet conventions)

The current cluster config has correctness-by-memory problems: the Prometheus
`ruleSelector` label, the Loki sidecar's `loki_rule` label, the severity
vocabulary, and the format→parser map are all conventions encoded nowhere. The
new code is structured so these are structurally impossible to violate.
Decisions: two new libs (not one), River config *generated* from data, required
alert fields kept minimal (summary + description only — no runbook/owner
boilerplate).

**`lib/alerts.libsonnet` — one alert DSL, two renderers.** Metric and log alerts
share one shape (name, expr, for, severity, summary, description) and differ
only in expr language and delivery. `alerts.rule(...)` asserts the severity
vocabulary and mandatory annotations. `alerts.prometheusRule(name, ns, rules)`
stamps the `release: kube-prometheus-stack` label the ruleSelector requires.
`alerts.lokiRule(name, ns, rules)` renders a ConfigMap with the `loki_rule`
label (via `std.manifestYamlDoc`) for the sidecar. Both magic labels become
un-forgettable.

**Colocation convention:** alerts live in the block of the thing they watch
(the argocd rules already do this — make it the rule, not an accident).
Cross-cutting concerns (journal, comin, meta-alerts) get one top-level
`observability:` block in `main.jsonnet`.

**`lib/logformats.libsonnet` — the parser map as data.** One `formats:: [...]
table (name, detection regex, level mapping, owning apps) plus
`renderProcess()`, which generates the alloy `loki.process` River block. The
alloy `configMap.content` stops being a heredoc. A new app/parser = one table
entry. The generator also unconditionally emits the `level="unknown"` coverage
signal, so no parser can be added without it. Phase 4's "document the
format→parser map" disappears as a task: `renderDocs()` generates the markdown
table from the same data. Risk noted: River syntax errors surface at alloy
startup, not at `tk export` — mitigated by eyeballing the configmap diff before
push and existing pod-crash alerts.

**`lib/config.libsonnet` grows a machine inventory** (`machines: { nuc: { lan,
wg, tailscale }, nas: {...}, saoiste: {...}, eachtrai: {...} }`). Scrape jobs
are generated from it: comin (lab machines by LAN, laptops by tailscale, port
4243) and the existing `host-node-exporter` static config — killing the
hand-maintained target list + relabel pair at `main.jsonnet:690-705`. The Nix
side keeps its own copy in `role/lab.nix`; crossing the nix/jsonnet boundary
isn't worth it — one source of truth per side.

**`serviceMonitor.new(name, ns, matchLabels, port)` helper** — collapses the
three copy-pasted argocd ServiceMonitors into one-liners; blocky's is a fourth.
Lives in `lib/alerts.libsonnet` or its own `lib/monitoring.libsonnet` — decide
at implementation.

**Alertmanager routes stay in kube-prometheus-stack values** but are extracted
to a `local alertmanagerRoutes = [...]` table with comments — one config
section, not worth a lib.

**healthchecks reuses `selfhosted.new()`** plus `+:` extension for the SMTP
secret env — the existing convention working as intended, no new pattern.

---

## Phase 0 — Volume control (first; ~100x reduction, zero risk)

| Source | Action | Expected result |
|---|---|---|
| storage-test pod | Retire the leftover NFS test pod (or convert to a real Job/CronJob so the file stop growing). It's `storageTest` in `cluster/environments/lab/main.jsonnet`; the write loop `cat`s the accumulated file every restart. | −232k lines/48h |
| traefik access | Filter at source: `accessLog.filters.statusCodes` in the traefik chart values, keeping only 4xx/5xx — removes the I/O entirely. (Ingest-side fallback if needed: `stage.json` then `stage.drop` with `source: DownstreamStatus`, regex `[123][0-9][0-9]`; `stage.drop` has no numeric comparison, so `>= 400` must be expressed as a regex on the extracted value.) Traefik metrics are already scraped (`podMetricsEndpoints` in the traefik chart values) — logs are redundant for status visibility. | −147k lines/48h |
| blocky queryLog | Disable at source (`queryLog.type: none` in blocky config), and add a ServiceMonitor for blocky's built-in metrics (currently unscraped) so query-level visibility survives. Source-side elimination beats an ingest-side drop rule — same argument as retiring WireGuard instead of alerting on it. | −112k lines/48h |

Expected steady state: ~500k → **~15k lines/48h**.

---

## Phase 1 — Ingest normalization (Alloy)

Replace the single JSON stage in `alloy-configmap.yaml` with ordered per-format matchers feeding one canonical schema:

| Format | Matcher | Extracts |
|---|---|---|
| JSON | `stage.json` | level, msg → message |
| logfmt | `stage.logfmt` | level, msg → message |
| `[timestamp] LEVEL text` (blocky, media, vaultwarden, coredns) | `stage.regex` | level |
| `timestamp INFO text` (ntfy) | `stage.regex` | level |
| `[I]/[W]/[E]` (forgejo/gitea) | `stage.regex` | level |
| Redis-style (immich valkey) | `stage.regex` on known markers | best-effort |

Schema rules:

- **Canonical levels:** `debug/info/warn/error/fatal` — one normalize template (`ToLower` + warn←warning/ERR map). Fixes the argocd `level=warning` misdetection.
- **`level` as a label** (bounded cardinality); **`message` as structured metadata** (Loki 3.x; never a label — unbounded).
- **Check `unit` label cardinality on host journals before locking the label set.** Both alloy (`discovery.relabel "journal"` in `alloy-configmap.yaml`) and nas's alloy journal relabel (incident remediation T2) map `__journal__systemd_unit` → `unit`. Every k3s pod on nuc spawns transient `run-*.scope`/`kubepods-*.scope` units, which churn the label. Measure series counts on `{job="systemd-journal"}` and add a relabel drop for `unit=~".*\\.scope"` if warranted.
- **Keep the raw line**; message is for alert context, not replacement.
- **Coverage signal:** emit `level="unknown"` counts per stream. A rule "unknown-level fraction rising" catches parser regressions so alerts can never silently stop matching.

---

## Phase 2 — Missing sources

1. **friendlywrt host logs** — `loki.source.syslog` in alloy + one uci change on
   the box (`system.@system[0].log_ip`/`log_proto`). Brings in OpenWrt syslog: k3s
   agent, sysntpd, firewall events. Two implementation requirements:
   - **`log_proto = 'tcp'`, not UDP.** UDP syslog drops silently under loss — an
     ironic failure mode for a plan about silent log loss.
   - **The alloy listener must be reachable from friendlywrt.** alloy runs as a
     daemonset; `loki.source.syslog` needs a hostPort (or a Service) on the
     syslog port, which the current daemonset doesn't have.
2. **Kubernetes events** — `loki.source.kubernetes_events` in alloy. OOMKills,
   evictions, scheduling failures are today's biggest blind spot. Requires RBAC
   (list/watch on events) that the alloy service account doesn't currently have —
   add the ClusterRole with the source or the rollout stalls on 403s.

---

## Phase 3 — Alerting

### Systemd unit alerts ("like journalctl")

Two signal paths. Only the nuc journal actually flows today; the nas journal is blocked on the transport remediation (`INCIDENT-2026-08-wireguard-transport.md`).

**Metrics → PrometheusRule** (`release: kube-prometheus-stack` label, same pattern as the ArgoCD rules):

| Alert | Expression | Notes |
|---|---|---|
| ~~`SystemdUnitFailed`~~ | — | **Already exists.** kube-prometheus-stack ships `NodeSystemdServiceFailed` (`kube-prometheus-stack-node-exporter-prometheusrule.yaml:326-335`) with an identical expression and `for: 5m`. The systemd collector is enabled on both hosts (`machines/nuc/default.nix:203`, `machines/nas/default.nix:216`), both scrape as `job="node-exporter"`, and the Alertmanager route has no severity filter. So mailsort should have notified. The work here is **diagnosing why it didn't**, not authoring the rule again. |
| `CriticalUnitNotActive` | `node_systemd_unit_state{name=~"k3s.service\|comin.service\|tailscaled.service\|jellyfin.service\|mosquitto.service\|systemd-timesyncd.service\|alloy.service",state=~"failed\|inactive"} == 1` | `for: 2m`; deploy/network-critical set. Includes `alloy.service` (the nas log shipper after incident remediation T2) — the pipeline must observe itself. Caveat: a normal restart passes through `inactive` briefly, so frequently-restarting units may flap at 2m; lengthen `for:` if that shows up |

**Journal → Loki ruler** (blocked: the ruler has no `alertmanager_url` — see audit and sequencing step 6):

| Alert | Expression | Notes |
|---|---|---|
| `JournalUnitFailure` | `{job="systemd-journal"} \|~ "entered failed state\|Failed to start\|Failed with result"` | fires on the event with the full message (unit + reason) |
| `JournalErrorRate` | `sum by (host, unit) (rate({job="systemd-journal",level="err"}[5m])) > 0.05` for 10m | error storms, not one-offs. Threshold must be tuned against the real err-line baseline first — `> 0` fires on a single sustained err line, and k3s/NetworkManager emit those routinely |

Caveats (learned from data):

- **Oneshot units** (mailsort): resting state is `inactive` — alert `failed` only. Long-running services: `failed\|inactive`. **Verified 2026-09-01 — mailsort non-notification root-caused, not a delivery-path bug:** mailsort is a oneshot on a 1-minute timer (`role/mailsort.nix`); its `state="failed"` is transient (one 5-min sample in 24h in Prometheus) and never satisfies `NodeSystemdServiceFailed`'s `for: 5m`. The metric exists, is scraped (`job="node-exporter"`), Alertmanager is up (`up{job="kube-prometheus-stack-alertmanager"} == 1` on both ports), and `ALERTS{alertname="NodeSystemdServiceFailed"}` is empty — the rule simply never met its `for`. A `for: 5m` metric rule is structurally wrong for a 1-min-retried oneshot; the journal-based `JournalUnitFailure` rule (below) is the correct signal for this class.
- **friendlywrt has no systemd/journal** — its k3s agent death is partially covered by `KubeNodeNotReady`; host-level failures need Phase 2's syslog.
- **Alertmanager routing:** journal alerts carry no `namespace` (route groups by namespace → empty group) and the default `repeat_interval: 12h` re-notifies slowly for repeatedly failing units. Add a route matcher (`alertname =~ "Systemd.*|Journal.*"`) with a shorter repeat interval.
- Live example found during audit: **mailsort.service failed within the last 7 days** ("Failed with result 'exit-code'") — nobody was notified. Worth checking why before enabling rules blindly.

### Meta-alerts (dead-man coverage)

Both August incidents were silence failures, and the mailsort non-notification is a
suspected broken delivery path *today*. Every rule in Phase 3 inherits that trust
problem: a rule that never fires is indistinguishable from a rule that can't
notify. Fix: a dead-man switch that expects a daily ping and complains when it
doesn't arrive.

**Design:** self-hosted healthchecks instance in-cluster (new app in
`main.jsonnet`; SQLite on a PVC, or a small cnpg database). Alertmanager gets a
dedicated route + receiver for `alertname="Watchdog"` (always-firing, ships with
kube-prometheus-stack) that webhook-POSTs to the healthchecks ping URL — same
webhook pattern as the ntfy receiver (`main.jsonnet:754-756`). Check period 24h +
grace.

**The watcher's alert path must not share the leg it's watching.** healthchecks
notifies on a missed ping via two channels: ntfy (primary) *and* Fastmail SMTP
(`smtp.fastmail.com` app password, Secret via the repo's existing pattern). The
Fastmail mailbox is fully external, so an ntfy death — the most likely broken leg,
and the live mailsort suspect — is still reported, by email.

| Failure | Detected by |
|---|---|
| Prometheus rule eval dead / Alertmanager wedged / route misconfig | ping stops → healthchecks alerts via ntfy + email |
| ntfy dead / token rot / webhook template broken | ping stops (webhook errors) → healthchecks alerts via **email** |
| healthchecks itself dead | existing kube-prometheus-stack pod-health rules |
| whole cluster dead | self-announcing (out of scope by decision) |

Once step 6 wires the Loki ruler, add a **second healthchecks check** pinged by an
always-true Loki ruler rule, so both alert engines get dead-man coverage. Same
pattern, second ping URL.

Bonus: no daily phone ping — the system is silent when healthy, avoiding
heartbeat-fatigue entirely.

### ArgoCD reconcile staleness

The existing four argocd rules (`argocd-prometheusrule.yaml`: `ArgoCDSyncFailed`,
`ArgoCDAppOutOfSync`, `ArgoCDAppDegraded`, `ArgoCDAppMissing`) are all
failure/state-based — none catch a wedged controller, where activity simply stops.
Verified against the live cluster (2026-09-01):

- The controller ServiceMonitor is working: `argocd_app_reconcile_count` is in
  Prometheus as `job="argocd-application-controller-metrics"`.
- Healthy cadence is **~22 reconciles/hour, regular** (~6 per 15min sustained over
  3h), consistent with `timeout.reconciliation: 120s` + 60s jitter in `argocd-cm`.

Add to `argocd-prometheusrule.yaml`:

| Alert | Expression | Notes |
|---|---|---|
| `ArgoCDReconcileStalled` | `sum(increase(argocd_app_reconcile_count[30m])) == 0` for 15m | Detects a wedged controller within ~45min. Normal cadence is ~10 per 30m, so a zero window is unambiguous; counter resets on controller restart are handled by `increase()` |

Honest caveat: without postmortem data from August, we can't reconstruct whether
incident #1's PreSync deadlock stopped reconciles entirely or only blocked syncs
while status refresh continued. The two rule shapes are complementary either way:
`ArgoCDAppOutOfSync` (already shipped) catches blocked-sync-with-live-controller;
`ArgoCDReconcileStalled` catches a wedged controller.

### Comin deploy visibility (metrics, verified 2026-09-01)

26 days of comin failure on saoiste surfaced nowhere. comin 0.14.0 (pinned rev
`c32a4e4`) ships a Prometheus exporter, **already live on every machine** (default
port 4243, all interfaces — confirmed serving on nuc from the LAN) with exactly
the right gauges:

- `comin_last_deployment_failed`, `comin_last_eval_failed`, `comin_last_build_failed`, `comin_last_fetch_failed{remote}`
- `comin_deployment_info{commit_id, status}` — currently deployed commit
- `comin_is_suspended`, `comin_need_to_reboot`

Implementation:

1. `role/comin.nix`: set `exporter.port = 4243` explicitly and
   `exporter.openFirewall = true` (no-op on nas where the firewall is off).
2. `main.jsonnet` `additionalScrapeConfigs`: add a `comin` job — nuc/nas by LAN
   address; saoiste (`100.64.0.1`) and eachtrai (`100.64.0.7`) by tailscale IP.
3. **TargetDown caveat:** the shipped `TargetDown` rule
   (`kube-prometheus-stack-general.rules-prometheusrule.yaml`) fires when >10% of
   a job's targets are down. Laptops sleep, so one offline laptop = 50% down for
   the `comin` job. Patch the rule in jsonnet to exclude `job="comin"`, or split
   laptops into their own job and exclude that. Do not disable `TargetDown`
   wholesale — it guards every other job.
4. Rules (new PrometheusRule, `release: kube-prometheus-stack` label):

| Alert | Expression | Notes |
|---|---|---|
| `CominDeploymentFailed` | `comin_last_deployment_failed == 1` for 5m | the direct signal |
| `CominFetchFailed` | `comin_last_fetch_failed == 1` for 1h | 1h grace for transient network loss |
| `CominNeedToReboot` | `comin_need_to_reboot == 1` for 1h | informational; deploys pending reboot |

Honest caveat: whether the frozen-PID-1 path on saoiste actually set
`comin_last_deployment_failed` is unverified (the deploy may have hung rather
than failed cleanly). The overlapping coverage — this gauge, `CominFetchFailed`,
the journal-based comin error rule, and `CriticalUnitNotActive` — means no single
assumption has to hold.

Coverage note: this is the first observability saoiste and eachtrai get at all —
they have no node-exporter and ship no logs today.

### Log-based alerts (after Phase 1 normalization)

| Alert | Signal | Depends on |
|---|---|---|
| `LogErrorRate` | rate of `level="error"` per (namespace, container) over 5–15m | Phase 1 |
| `LogAbsence` | no logs from a watched stream for N minutes (silently dead service) | Phase 1 |
| `LogVolumeAnomaly` | per-stream count deviation (would have caught storage-test growth) | Phase 1 |
| `ParseCoverage` | fraction of lines with `level="unknown"` rising | Phase 1 (guard against regressions) |
| `K8sEventWarning` | warning/error events from Phase 2 source | Phase 2 |

---

## Phase 4 — Hygiene

- "Log health" dashboard: volume per stream, level coverage per stream, unknown-level count.
- Bounded label set: `namespace, container, app, level, unit, host`; everything else → structured metadata.
- Retention bump (Phase 0 post-check): with volume cut ~100x, raise Loki retention (`loki-configmap.yaml:52`) from 720h to 90d — the capacity constraint that forced 720h is gone.
- Document the format→parser map in the repo (patterns over one-offs).
- Test new rules against the existing ntfy topic with synthetic log lines before relying on them.

---

## Explicitly not recommended

- **Per-alert parsing in LogQL** — one normalization at ingest or the coverage guarantees fall apart.
- **`message` as a label** — index cardinality blowup.
- **Per-service bespoke pipelines** — one pipeline with matchers; a new app is one regex line.
- **Log alerts before Phase 1** — every rule written against today's parsing on a bracket-format service silently never fires.

---

## Backlog (execution order)

Implementation state as of 2026-09-01. `[x]` = code written and rendered
(`tk export` + `alloy fmt`/nix build clean); deployment-loop verification is a
separate pass after git push.

- [ ] **0. Transport remediation** — T1/T2/T5–T10 done (validated); T3 config
  written but blocked on a manual agenix secret; T4 is the manual saoiste
  reboot. Checklist: `INCIDENT-2026-08-wireguard-transport.md`.
- [x] **1. Phase 0 — volume control** — storage-test retired; traefik
  `accessLog.filters.statusCodes: '400-599'`; blocky `queryLog.type: none`
  (its metrics were already scraped — the plan's "unscraped" note was stale).
- [x] **2. Diagnose the mailsort non-notification.** Verified: `for: 5m` vs a
  1-min-retried oneshot; metric/scrape/Alertmanager all healthy; no delivery
  fault.
- [ ] **3. Meta-alerts, part 1** — healthchecks app + `Watchdog` route written;
  blocked on MANUAL steps (sops secret, `createsuperuser`, create check +
  channels in the UI, paste UUID into the receiver).
- [x] **4. Wire the Loki ruler** — `alertmanager_url`,
  `storage.local.directory: /rules`, `enable_alertmanager_v2: true` all render
  into `loki-configmap.yaml`.
- [x] **5. `CriticalUnitNotActive`** — `lib/alerts.libsonnet` built;
  `node-units` PrometheusRule renders.
- [x] **6. `ArgoCDReconcileStalled`** — added; argocd rules ported to
  `alerts.rule`; three ServiceMonitors collapsed via `alerts.serviceMonitor`.
- [x] **7. Comin visibility** — `config.libsonnet` machine inventory; comin
  scrape job generated; `TargetDown` patched (`job!="comin"`); three comin
  rules; `role/comin.nix` exporter config.
- [x] **8. Phase 1 — ingest normalization** — `lib/logformats.libsonnet` built;
  River generated from the format map and helm-escaped; coverage signal emits
  `level="unknown"`. Selectors seeded from the audit inventory — **tune against
  the `level="unknown"` fraction in the deploy loop**. `unit` cardinality check
  left as a deploy-loop measurement.
- [x] **9. Phase 2 — missing sources** — friendlywrt syslog (TCP on hostPort
  5140) + k8s events, both in the generated alloy config (RBAC already ships
  with the chart). The friendlywrt-side uci change is manual (box not in this
  repo).
- [ ] **10. Log-based rules** — `JournalUnitFailure`/`JournalErrorRate`/
  `LogErrorRate`/`LogAbsence`/`ParseCoverage` written as Loki-ruler ConfigMaps.
  `K8sEventWarning` and `LogVolumeAnomaly` deferred: the former needs the
  events component's emitted labels verified in the deploy loop (couldn't be
  verified offline), the latter needs a recording-rule baseline.
  Meta-alerts part 2 (Loki-ruler healthchecks check) blocked on item 3.
- [ ] **11. Phase 4 — hygiene** — retention 2160h (90d) done; log-health
  dashboard and the generated format→parser doc (`logformats.renderDocs()`)
  still to land.

Deployment-loop verification still required (can't be done from here): nas
journal appearing in Loki after T1+T2 deploy; Loki ruler picking up the
`loki-rule-*` ConfigMaps and reaching Alertmanager; per-format level coverage
on real logs; the comin scrape on live saoiste/eachtrai.

All changes are declarative. Cluster side: `cluster/environments/lab/main.jsonnet` + rendered manifests (alloy config, PrometheusRules, Loki ruler config, Alertmanager route) through git → ArgoCD. Host side: `role/lab.nix`, `role/mpd.nix`, `machines/*/default.nix` through git → comin. The unavoidable manual steps live in the incident doc: minting a Tailscale auth key, the tailnet stale-node cleanup, the `saoiste` reboot, the healthchecks secret/superuser/check setup, and the friendlywrt uci syslog change.
