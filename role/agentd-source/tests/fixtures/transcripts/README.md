# Captured terminal records

Captured on 2026-09-26 from the isolated live harness runner, with Claude Code
2.1.282 and Codex CLI 0.156.1. Claude records come from the same run as the hook
fixtures; Codex records come from a subsequent run with `ephemeral:false`.

Only relevant records were selected. Conversation and turn/prompt IDs have
been replaced, Claude working-directory/git fields removed, and Codex session
metadata reduced to its ID and CLI version (the original includes the full
system prompt). Other payload fields retain their captured shapes. All prompts,
errors, and model responses belong to the local test endpoint.

`tests/transcript-tests.c` tests these through the actual incremental reader,
including missing hooks, shutdown, replay, partial writes and turn isolation.
`tests/server.bats` additionally checks recovery after a real daemon restart.
`make check-live` requires these statuses from the actual harnesses; the driver
does not inject synthetic status events into agentd.
