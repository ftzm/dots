#include "session.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

static struct state state;
static int64_t now = 1000;

static void hook(struct session *s, const char *name, const char *turn)
{
    cJSON *event = cJSON_CreateObject();
    json_put(event, "agent_session", cJSON_CreateString(s->id));
    json_put(event, "agent_kind", cJSON_CreateString(s->kind));
    json_put(event, "session_id", cJSON_CreateString(s->conversation_id));
    json_put(event, "hook_event_name", cJSON_CreateString(name));
    if (turn) json_put(event, text_is(s->kind, "claude") ? "prompt_id" : "turn_id", cJSON_CreateString(turn));
    struct session *changed;
    assert(session_event(&state, event, now++, &changed) >= 0);
    cJSON_Delete(event);
}

static void append(const char *path, const char *text)
{
    FILE *out = fopen(path, "a");
    assert(out && fputs(text, out) >= 0 && fclose(out) == 0);
}

static void fixture(const char *path, const char *name)
{
    char file[256];
    snprintf(file, sizeof(file), "tests/fixtures/transcripts/%s.jsonl", name);
    FILE *in = fopen(file, "r");
    assert(in);
    char *line = NULL;
    size_t capacity = 0;
    while (getline(&line, &capacity, in) >= 0) append(path, line);
    assert(!ferror(in));
    fclose(in); free(line);
}

static void fresh_file(const char *path)
{
    char replacement[256];
    snprintf(replacement, sizeof(replacement), "%s.new", path);
    FILE *out = fopen(replacement, "w");
    assert(out && fclose(out) == 0);
    assert(rename(replacement, path) == 0);
}

int main(void)
{
    char path[] = "/tmp/agentd-transcript.XXXXXX";
    int fd = mkstemp(path);
    assert(fd >= 0); close(fd);
    struct session *s = session_add(&state, "one", "codex", "/test", NULL, now++);
    replace_string(&s->conversation_id, "codex-conversation");
    replace_string(&s->transcript_path, path);
    hook(s, "UserPromptSubmit", "failed");
    fixture(path, "codex-failure");
    assert(transcript_poll(s, now++) && s->status == ERROR && s->turn_finished);
    assert(strstr(s->message, "login required") && !s->observation_error);
    assert(!transcript_poll(s, now++)); /* No repeated broadcasts/writes at EOF. */
    hook(s, "Stop", "failed");
    assert(s->status == ERROR);
    hook(s, "UserPromptSubmit", "cancelled");
    assert(s->status == WORKING && !s->turn_finished);
    transcript_poll(s, now++);
    assert(s->status == WORKING); /* Previous error cannot undo recovery. */
    fixture(path, "codex-interrupted");
    assert(transcript_poll(s, now++) && s->status == IDLE && s->turn_finished);
    hook(s, "PostToolUse", "cancelled");
    assert(s->status == IDLE); /* Delayed same-turn hook cannot undo cancellation. */
    hook(s, "PreToolUse", "failed");
    assert(s->status == IDLE);

    /* Restart with unread durable completion and no new hook. */
    hook(s, "UserPromptSubmit", "next");
    cJSON *saved = state_json(&state);
    assert(state_decode(&state, saved)); cJSON_Delete(saved);
    s = session_find(&state, "one");
    assert(text_is(s->turn_id, "next") && !s->turn_finished);
    append(path, "{\"type\":\"event_msg\",\"payload\":{\"type\":\"task_complete\",\"turn_id\":\"next\"}}");
    transcript_poll(s, now++);
    assert(s->status == WORKING); /* No newline: write is incomplete. */
    append(path, "\n");
    assert(transcript_poll(s, now++) && s->status == IDLE);

    /* Reader replacement/truncation and wrong conversation never reuse context. */
    fresh_file(path);
    hook(s, "UserPromptSubmit", "failed");
    append(path, "{\"type\":\"session_meta\",\"payload\":{\"id\":\"another\"}}\n");
    fixture(path, "codex-interrupted");
    assert(transcript_poll(s, now++) && s->status == WORKING && s->observation_error);
    fresh_file(path);
    fixture(path, "codex-failure");
    assert(transcript_poll(s, now++) && s->status == ERROR && !s->observation_error);

    /* Claude: failure without StopFailure, even after SessionEnd. */
    fresh_file(path);
    replace_string(&s->kind, "claude");
    replace_string(&s->conversation_id, "claude-conversation");
    hook(s, "UserPromptSubmit", "other");
    hook(s, "UserPromptSubmit", "failed");
    hook(s, "SessionEnd", "failed");
    assert(s->status == EXITED);
    fixture(path, "claude-failure");
    assert(transcript_poll(s, now++) && s->status == ERROR && strstr(s->message, "login required"));
    hook(s, "UserPromptSubmit", "cancelled");
    fixture(path, "claude-interrupted");
    assert(transcript_poll(s, now++) && s->status == IDLE);
    hook(s, "PreToolUse", "cancelled");
    assert(s->status == IDLE);

    /* Child, previous-turn, and different-conversation records are irrelevant. */
    hook(s, "UserPromptSubmit", "current");
    append(path, "{\"type\":\"assistant\",\"isSidechain\":true,\"sessionId\":\"claude-conversation\",\"promptId\":\"current\",\"isApiErrorMessage\":true}\n");
    append(path, "{\"type\":\"assistant\",\"isSidechain\":false,\"sessionId\":\"another\",\"promptId\":\"current\",\"isApiErrorMessage\":true}\n");
    fixture(path, "claude-failure");
    transcript_poll(s, now++);
    assert(s->status == WORKING);

    /* Missing and malformed transcripts are visible, never inferred completion. */
    assert(unlink(path) == 0);
    assert(transcript_poll(s, now++) && s->observation_error && s->status_stale && s->status == WORKING);
    assert(!transcript_poll(s, now++));
    append(path, "{not JSON}\n");
    assert(transcript_poll(s, now++) && strstr(s->observation_error, "parse"));
    assert(!transcript_poll(s, now++));
    assert(unlink(path) == 0);
    /* Work and memory stay bounded even for an oversized record; resynchronize
       at newline and still read a subsequent valid terminal record. */
    transcript_reset(s);
    FILE *large = fopen(path, "w");
    assert(large);
    for (size_t i = 0; i < 1024 * 1024 + 20; i++) assert(fputc('x', large) != EOF);
    assert(fputs("\n", large) >= 0 && fclose(large) == 0);
    hook(s, "UserPromptSubmit", "cancelled");
    fixture(path, "claude-interrupted");
    for (int i = 0; i < 20; i++) transcript_poll(s, now++);
    assert(s->status == IDLE && s->status_stale && strstr(s->observation_error, "Oversized"));
    assert(!transcript_poll(s, now++));
    assert(unlink(path) == 0);
    state_free(&state);
    puts("PASS: captured terminal records, missed hooks, restart, partial writes, turn isolation and reader failures");
    return 0;
}
