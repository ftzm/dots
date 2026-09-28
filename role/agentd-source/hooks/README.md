# Harness hook definitions

These files are the authoritative hook registrations used by this project:

- `claude.json`: Claude Code settings containing command hooks.
- `codex.json`: Codex native `hooks.json` configuration, using lifecycle hooks.
- `versions.json`: harness versions used for the verification recorded below.

Edit these files when changing registrations. Tests and `agent-new` load the
same definitions rather than maintaining separate copies.
The state adapters and their limits are described in
[server.md](../docs/server.md), with current coverage and limits in
[status-coverage.md](../docs/status-coverage.md). Registering an event does not
establish its state-transition semantics.

## Version support

The recorded baselines are **Claude Code 2.1.282** and **Codex CLI 0.156.1**.
Other versions are allowed. A version mismatch is at most a warning and must
not prevent launch or testing. These baselines record evidence, not an allowlist.
See [live verification](../docs/hook-verification.md) for tested lifecycle coverage
and the supplemental transcript signals needed where hooks are absent. The
[September 27 follow-up](../docs/status-coverage.md) extends those original checks.

| Harness | Verification at the recorded version |
| --- | --- |
| Claude Code | Live startup, prompt, permission, structured input, tool failure, StopFailure/recovery, foreground and concurrent background children, MCP elicitation, cancellation, shutdown, clear and resume checked through agentd. |
| Codex CLI | All definitions discovered and trusted by exact hashes; live prompt, permission, structured input, tool failure, cancellation, completion, shutdown, concurrent children and CLI delivery checked. API failures require transcript evidence. Trusted MCP elicitation has no observed hook/transcript signal. |

`make check-live` prints both installed versions, warns if either differs from
`versions.json`, and runs the same behavior checks regardless. Actual hook
behavior determines whether the test passes. The development flake currently uses the workstation's agent executables;
it does not pin or install either harness.

No hook-specific semantic-versioning guarantee was established from the
[Claude hook reference](https://code.claude.com/docs/en/hooks) or the
[official OpenAI hook documentation](https://learn.chatgpt.com/docs/hooks).
Do not infer a compatibility range solely from the CLI version's shape. If an
upstream compatibility guarantee is documented, use that range for warnings.

When updating a recorded baseline, review its hook interface, rerun the applicable
runtime checks, and update this verification record along with `versions.json`.
Do not broaden support based only on successful JSON parsing.

## Environment and loading

The harness must inherit:

| Variable | Value |
| --- | --- |
| `AGENT_HOOK_FORWARD` | Absolute path to `bin/agent-hook-forward` |
| `AGENT_SESSION` | Immutable managed session ID |
| `AGENTD_SOCKET` | Optional socket override; defaults to `$XDG_RUNTIME_DIR/agentd/ingest.sock` |

The forwarder also accepts `ZMX_SESSION` as a fallback association. Each
registration supplies the harness kind explicitly. Quoting supports forwarder
paths containing spaces. The hook commands suppress output and return success
even if the forwarder is missing. The forwarder bounds its worker to 200 ms;
the harness-level timeout is one second.

Claude can load `claude.json` with `--settings /absolute/path/to/hooks/claude.json`.
Codex supports `hooks.json` alongside its configuration layers and requires hook
trust. `agent-new` supplies these definitions through per-invocation config,
discovers their hashes with a short-lived app server, and verifies trust for
exactly those definitions before launching the native TUI with `--no-daemon`.
Nothing here installs or replaces the user's global configuration automatically.

## Checks

`make check-hooks` invokes every registered command with a fixture and checks
delivery through the forwarder into C/cJSON, including harness identity, path
quoting, and silent failure. It is also included in `make check`.

`make check-claude` runs the actual Claude executable with these registrations,
adding only a test-specific prompt blocker. See the
[probe notes](../docs/hook-probe.md) for isolation details and remaining limits.

`make check-live` exercises both real harnesses against a loopback-only scripted
HTTP endpoint, using temporary configuration and native Codex trust. No paid model
requests or existing user credentials are involved. It also checks that agentd
recovers missing status signals from the harness transcripts. Both native TUIs
are also launched through `agent-new` under zmx to verify per-launch hooks,
completion, detach, reattach, and explicit kill.
