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
- **Host journal:** only from `nuc` (NixOS). Labeled `unit`, `level` (systemd priority), `host`, `syslog_identifier`. `nas` journal is not shipped.
- **OTLP traces → Tempo**, OTLP logs not used.
- **Loki:** ruler enabled (local rules dir), filesystem storage, 30d retention, memcached chunk cache, `reject_old_samples_max_age: 168h`.
- **Alerting:** kube-prometheus-stack metric rules (node/pod/deployment/job level) + newly added ArgoCD rules → Alertmanager → ntfy. **No log-based rules, no Grafana-managed rules, no k8s events.**

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

## Phase 0 — Volume control (first; ~100x reduction, zero risk)

| Source | Action | Expected result |
|---|---|---|
| storage-test pod | Retire the leftover NFS test pod (or convert to a real Job/CronJob so the file stop growing). It's `storageTest` in `cluster/environments/lab/main.jsonnet`; the write loop `cat`s the accumulated file every restart. | −232k lines/48h |
| traefik access | `stage.drop` in alloy unless `DownstreamStatus >= 400`. Traefik metrics are already scraped (`podMetricsEndpoints` in the traefik chart values) — logs are redundant for status visibility. | −147k lines/48h |
| blocky queryLog | Drop unless `response_code != NOERROR`. Optional: add a ServiceMonitor for blocky's built-in metrics (currently unscraped), then queryLog becomes fully redundant. | −112k lines/48h |

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
- **Keep the raw line**; message is for alert context, not replacement.
- **Coverage signal:** emit `level="unknown"` counts per stream. A rule "unknown-level fraction rising" catches parser regressions so alerts can never silently stop matching.

---

## Phase 2 — Missing sources

1. **friendlywrt host logs** — `loki.source.syslog` in alloy (UDP listener) + one uci change on the box (`system.@system[0].log_ip`/`log_proto`). Brings in OpenWrt syslog: k3s agent, sysntpd, firewall events.
2. **Kubernetes events** — `loki.source.kubernetes_events` in alloy. OOMKills, evictions, scheduling failures are today's biggest blind spot.

---

## Phase 3 — Alerting

### Systemd unit alerts ("like journalctl")

Two signal paths, both already flowing today (nuc + nas):

**Metrics → PrometheusRule** (`release: kube-prometheus-stack` label, same pattern as the ArgoCD rules):

| Alert | Expression | Notes |
|---|---|---|
| `SystemdUnitFailed` | `node_systemd_unit_state{state="failed"} == 1` | `for: 5m` (units flap); any failed unit |
| `CriticalUnitNotActive` | `node_systemd_unit_state{name=~"k3s.service\|comin.service\|tailscaled.service\|jellyfin.service\|mosquitto.service\|systemd-timesyncd.service",state=~"failed\|inactive"} == 1` | `for: 2m`; deploy/network-critical set |

**Journal → Loki ruler** (ruler already enabled, local rules dir):

| Alert | Expression | Notes |
|---|---|---|
| `JournalUnitFailure` | `{job="systemd-journal"} \|~ "entered failed state\|Failed to start\|Failed with result"` | fires on the event with the full message (unit + reason) |
| `JournalErrorRate` | `rate(count_over_time({job="systemd-journal",level="err"}[5m])) > 0` for 10m | error storms, not one-offs |

Caveats (learned from data):

- **Oneshot units** (mailsort): resting state is `inactive` — alert `failed` only. Long-running services: `failed\|inactive`.
- **friendlywrt has no systemd/journal** — its k3s agent death is partially covered by `KubeNodeNotReady`; host-level failures need Phase 2's syslog.
- **Alertmanager routing:** journal alerts carry no `namespace` (route groups by namespace → empty group) and the default `repeat_interval: 12h` re-notifies slowly for repeatedly failing units. Add a route matcher (`alertname =~ "Systemd.*|Journal.*"`) with a shorter repeat interval.
- Live example found during audit: **mailsort.service failed within the last 7 days** ("Failed with result 'exit-code'") — nobody was notified. Worth checking why before enabling rules blindly.

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
- Document the format→parser map in the repo (patterns over one-offs).
- Test new rules against the existing ntfy topic with synthetic log lines before relying on them.

---

## Explicitly not recommended

- **Per-alert parsing in LogQL** — one normalization at ingest or the coverage guarantees fall apart.
- **`message` as a label** — index cardinality blowup.
- **Per-service bespoke pipelines** — one pipeline with matchers; a new app is one regex line.
- **Log alerts before Phase 1** — every rule written against today's parsing on a bracket-format service silently never fires.

---

## Sequencing

1. **Phase 0** — volume (~100x cost reduction, hours of work).
2. **Systemd unit alerts** (metrics + journal) — independent of Phase 1, closes the mailsort-class gap immediately.
3. **Phase 1** — normalization pipeline + coverage signal (prerequisite for trustworthy log alerting).
4. **Phase 2** — syslog + k8s events.
5. **Phase 3** — log-based rules, in the order: K8sEventWarning → JournalUnitError → LogErrorRate → LogAbsence → LogVolumeAnomaly → ParseCoverage.
6. **Phase 4** — dashboards and documentation.

All changes are declarative: `cluster/environments/lab/main.jsonnet` + rendered manifests (alloy config, PrometheusRules, Loki ruler config, Alertmanager route), through git → ArgoCD.
