# Captured hook fixtures

Captured on 2026-09-26 from Claude Code 2.1.282 and Codex CLI 0.156.1 using
`tests/harness-live.sh`. These are events emitted by the actual installed
harnesses, with model responses supplied by the local scripted HTTP endpoint.
No real model service was contacted. Request/response events from the harness
clients were also inspected to distinguish API failure, cancellation, and user
input from ordinary completion.

`tests/sanitize-hooks.jq` replaces temporary paths and generated conversation,
turn, prompt and agent IDs. Other hook fields retain their captured shapes.
The fixtures do not include API headers, keys, or existing user conversations.
`claude-background.jsonl` is the first six events from an additional exploratory
run where `Agent` omitted `run_in_background`. That run launched the child
asynchronously; the parent Stop still reported a running background task.
The main repeatable live suite explicitly requests a foreground subagent.

`cases.json` specifies the intended status after each captured event, replayed
in order through the real adapters by `tests/fixture-tests.c`. It includes
hook-only gaps: Codex API failure and early Claude cancellation produce no hook
that can move the record out of `working`. The supplemental reader now fills
those gaps; its captured [transcript fixtures](../transcripts/README.md) and live
checks verify the resulting error/idle states separately.

See [verification findings](../../../docs/hook-verification.md) for the scope,
observed limitations, and remaining tests.

## September 27 concurrency and failure captures

Added native Claude 2.1.282 and Codex 0.156.1 sequences for concurrent child
permissions/completions, HTTP 429/500 failures, Codex question cancellation and
Claude MCP cancellation. Captures came from the isolated local endpoint suite
(`/tmp/agentd-harness.iJt5a5`); paths are normalized under `/verification`.
Generated actor/turn IDs remain intact so cross-event associations are testable.
The earlier Claude background fixture's nested task ID was corrected to match
its sanitized SubagentStart ID. See `docs/status-coverage.md` for limits.
