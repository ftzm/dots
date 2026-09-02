# Incident 2026-08: WireGuard transport rot (nas logging outage, saoiste PID-1 freeze)

Investigated 2026-08-30. The `nas` journal gap found during the logging audit was
not a missing-source problem, it was a broken-transport problem — and the same
broken transport silently took out four other things. This doc is the postmortem
plus the remediation backlog. The observability build-out it motivated lives in
`LOGGING_PLAN.md`.

## Remediation backlog

**Start here: T1+T2** — live 32-day outage, ~35 lines of diff, nuc and nas are
healthy so comin deploys immediately. Verify: `{host="nas"}` lines appear in
Loki. T3 is unblocked but independent; T4 (manual reboot) gates T5–T10 and
nothing in `LOGGING_PLAN.md` — do it in the first session anyway, it's five
minutes of console access.

Declarative (git → comin), in order:

- [x] **T1.** `role/lab.nix:19` — `machines.nuc.wg` → `machines.nuc.lan`. Ends the
  nas→Loki outage.
- [x] **T2.** `machines/nas/default.nix` — promtail replaced with
  `services.alloy` (River config validated with `alloy fmt`; nas toplevel
  builds).
- [ ] **T3.** `machines/nas/default.nix` — `services.tailscale.enable` +
  agenix-backed `authKeyFile` (config written). MANUAL: mint a Tailscale auth
  key, run `agenix -e secrets/tailscale-authkey-nas.age`, clean the stale
  eachtrai nodes from the admin console (see Tailnet hygiene below).

**Blocked on the saoiste reboot** (nothing else is):

- [ ] **T4.** Reboot `saoiste` — the only way to clear a frozen PID 1.
  `systemctl reboot` will likely fail for the same reason; fallback: `sync`,
  then `reboot -f`. Unblocks 26 days of queued deploys including `9c94bb7`,
  which removes the freeze mechanism.
- [x] **T5.** `role/mpd.nix` — `device = "nas.tail.ftzmlab.xyz:/music"`.
- [x] **T6.** `role/mpd.nix` — mount options rebuilt for roaming.
- [x] **T7.** `role/mpd.nix` — `host "nuc.tail.ftzmlab.xyz"`.
- [x] **T8.** `machines/saoiste/default.nix:90` — buildMachines `wg-nuc` →
  `nuc.tail.ftzmlab.xyz`.
- [x] **T9.** `machines/saoiste/default.nix:314` — atuin →
  `http://nuc.tail.ftzmlab.xyz:8889`.
- [x] **T10.** `machines/eachtrai/default.nix:358` — atuin →
  `http://nuc.tail.ftzmlab.xyz:8889`.

Verification: nas journal lines visible in Loki with `host="nas"` (T1+T2);
`tailscale status` on nas (T3); comin deploys landing on saoiste again (T4+);
`/mnt/music` mounts ro with the new options and survives a suspend/resume
cycle (T5–T7).

## What is actually wrong

`nas` ships its journal to the cluster Loki via promtail
(`machines/nas/default.nix:221-262` → `lab.services.lokiPush`).
`role/lab.nix:19` resolves that to `http://10.0.100.4:30100/loki/api/v1/push` —
nuc's **WireGuard** address on the cluster loki-gateway NodePort. There is no
local Loki anywhere in the NixOS configs; the target was always the cluster.

| Evidence | Value |
|---|---|
| Loki `host` label values, full 30d retention window | `["nuc"]` — no nas data at all |
| `promtail_request_duration_seconds_count{status_code="-1"}` | 59,290 (transport failure) |
| `promtail_request_duration_seconds_count{status_code="204"}` | 1,784 |
| `promtail_dropped_entries_total{reason="ingester_error"}` | 86,788 |
| `promtail_sent_bytes_total` / `promtail_encoded_bytes_total` | 797 KB of 7.7 MB |
| nas ↔ nuc last WireGuard handshake | 2026-07-29 — **32 days ago** |

Both ends of the tunnel report the same handshake timestamp. Retention is 720h,
so the last successful pushes have aged out; the timeline is consistent end to end.

## The same tunnel took out `saoiste`

`saoiste` last handshaked with nuc ~26 days ago and has no `wg0` device at all.
From its journal:

```
Aug 04 06:44:01 saoiste systemd[1]: Failed to fork off sandboxing environment
                                    for executing generators: Protocol error
Aug 04 06:44:01 saoiste systemd[1]: Freezing execution.
Aug 04 06:45:55 saoiste comin[1102]: Error: systemd daemon reexecute failed, timeout after 180s
```

This is precisely the failure documented in `role/nfs-automount.nix:9-18`.
**PID 1 has been frozen for 26 days.** `/run/current-system` is dated Aug 4 06:42
— two minutes before the freeze — and `journalctl -b` still contains Aug 4
entries, so there has been no reboot since. comin has failed continuously
(`Failed to subscribe to systemd dbus messages`, most recently Aug 28).

Consequences, all silent: **no deploys for 26 days** — including `9c94bb7`, the
commit that fixes this exact bug class, which cannot be applied because applying
it requires the frozen D-Bus; `wireguard-wg0` never restarted, hence no `wg0`;
`/mnt/music` left as a stale `hard` NFS mount against an unroutable address; mpd
proxy, atuin sync and nix distributed builds to nuc all dead.

A frozen PID 1 is not recoverable in place — `daemon-reexec` needs the same dead
bus. **Only a reboot clears it**, and `systemctl reboot` will likely fail for the
same reason (fallback: `sync`, then `reboot -f`).

## Design

WireGuard is doing two unrelated jobs; only one should survive.

**Keep** — nuc's `wg0` as a *bind* address. Three cluster components depend on it:
`cluster/lib/config.libsonnet:5` (`wgIP`), `traefik-deployment.yaml:64,67`
(`wgweb`/`wgsecure` on `10.0.100.4:80/443`), and
`blocky-config-configmap.yaml:50` (DNS on `10.0.100.4:53`). If `wg0` left nuc,
cluster ingress would fail to bind. `leigheas` is also an active peer.

**Retire** — WireGuard as anything a service *dials*. Two replacements, by class:

1. **Lab-internal (nuc ↔ nas).** Both are permanently on `192.168.1.0/24`;
   reachability is not a variable. Use LAN addresses directly — no resolver, no
   per-machine class flags.
2. **Client → lab (saoiste, eachtrai, …).** Use Tailscale. It is
   location-independent *and* connects directly over the LAN when both ends are
   home — verified: `nuc.tail.ftzmlab.xyz → direct 192.168.1.4:41641`. One name is
   correct at home and away, so the consuming module needs no parameterization at
   all.

`role/mpd.nix` belongs to class 2, not class 1: it is imported by `saoiste` and
`eachtrai` — user devices, one stationary and one that travels — never by a lab
machine. That is why parameterizing it on lab topology was the wrong shape.

MagicDNS suffix is `tail.ftzmlab.xyz`. Short names resolve via `search`, but the
NFS device string uses the FQDN so it never depends on search-path ordering.

## Supporting detail

No supporting work needed for the NFS/atuin changes: nas exports to `*`
(`machines/nas/default.nix:128-133`) with `networking.firewall.enable = false`,
and nuc's mpd already binds `listenAddress = "any"`.

**Existing tailnet auth is not declarative.** All three enrolled machines are a
bare `services.tailscale.enable = true` (`nuc:557`, `saoiste:322`,
`eachtrai:413`) — registration was done by hand. nas should instead use an
agenix-backed `authKeyFile`, matching the repo's existing secret pattern
(`age.secrets.borgbase-key`, `wireguard-private-key-nas`), so enrolment happens
in the rebuild rather than as a manual `tailscale up`.

**Mount options.** Current
`["nfsvers=3" "noatime" "nodiratime" "rsize=32768" "async" "ro"]` has four
problems for a laptop that sleeps and roams: no `soft`, so it defaults to `hard`
and any process touching `/mnt/music` blocks in D-state indefinitely when the
link drops — the same hang class that cascaded into the PID-1 freeze;
`rsize=32768` is small for a WAN path, and the server already negotiates
`wsize=1048576`; no `actimeo`, so a near-static library pays attribute
revalidation round-trips that WAN latency multiplies; and NFSv3 has no session
recovery for a client that vanishes and returns.

```nix
options = [
  "nfsvers=4.2"                    # session recovery across sleep / network change
  "soft" "timeo=100" "retrans=3"   # fail in ~30s, never hang; safe because ro
  "ro" "noatime" "nodiratime"
  "rsize=1048576" "wsize=1048576"
  "actimeo=3600"
  "nconnect=4"
];
```

`machines/nas/default.nix:129` sets `fsid=root` on `/pool-1/`, so under NFSv4 the
pseudo-root *is* `/pool-1/` and the share mounts as `:/music`, not
`:/pool-1/music` — hence the device path change. (`nohide` on the subdir exports
is a v3-ism, implicit under v4.) If v4 proves troublesome on this export, the
smaller fallback is `nfsvers=3` with the `soft`/`timeo`/`actimeo`/`rsize`
additions and the original path.

## Tailnet hygiene (prerequisite for T3)

`tailscale status` shows three `eachtrai` registrations: the bare name `eachtrai`
is held by a node offline 151 days, while the live machine is
`eachtrai-5k2mrtvr`. Nothing addresses eachtrai by name, so this is harmless
today — but if `nas` ever re-registers it can silently become `nas-xxxx` and
break the mount. Clean the stale nodes out of the admin console before enrolling
nas.

## Why this beats alerting on it

The logging plan would answer a dead transport with a `LogAbsence` rule. Deleting
the dependency is strictly better: after these changes no service path traverses
a tunnel that can rot unnoticed, and WireGuard's remaining job — roaming ingress
to nuc — is self-announcing, because you notice when a device can't reach home.
That removes the failure class instead of instrumenting it.
