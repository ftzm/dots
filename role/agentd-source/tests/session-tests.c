#include "session.h"

#include <assert.h>
#include <stdio.h>
#include <string.h>

static int event(struct state *state, const char *json, int64_t now)
{
    cJSON *value = cJSON_Parse(json);
    assert(value);
    struct session *changed = NULL;
    int result = session_event(state, value, now, &changed);
    assert((result == 1) == (changed != NULL));
    cJSON_Delete(value);
    return result;
}

#define CLAUDE(fields) "{\"agent_session\":\"one\",\"agent_kind\":\"claude\",\"session_id\":\"c1\"," fields "}"
#define CODEX(fields) "{\"agent_session\":\"two\",\"agent_kind\":\"codex\",\"session_id\":\"x1\"," fields "}"

int main(void)
{
    const char *nul = "{\"id\":\"one\\u0000suffix\"}";
    assert(!json_parse(nul, strlen(nul)));
    const char *literal = "{\"title\":\"literal \\\\u0000 text\"}";
    cJSON *literal_value = json_parse(literal, strlen(literal));
    assert(literal_value);
    cJSON_Delete(literal_value);
    struct state state = {0};
    assert(event(&state, CLAUDE("\"hook_event_name\":\"Stop\""), 1) == 0);
    struct session *a = session_add(&state, "one", "claude", "/work", "My title", 10);
    struct session *b = session_add(&state, "two", "codex", "/other", NULL, 10);
    assert(a && b && state.count == 2);
    assert(!session_add(&state, "one", "claude", NULL, NULL, 10));
    assert(a->status == UNKNOWN && a->status_stale);
    assert(event(&state, CLAUDE("\"hook_event_name\":\"SessionStart\",\"transcript_path\":\"/first\""), 20) == 1);
    assert(a->status == IDLE && !a->status_stale && a->status_since == 20);
    assert(event(&state, CLAUDE("\"hook_event_name\":\"UserPromptSubmit\""), 30) == 1);
    assert(a->status == WORKING && b->status == UNKNOWN);
    assert(event(&state, CLAUDE("\"hook_event_name\":\"PreToolUse\""), 40) == 1);
    assert(a->status_since == 30 && a->last_event_at == 40);
    assert(event(&state, CLAUDE("\"hook_event_name\":\"PermissionRequest\""), 50) == 1);
    assert(a->status == WAITING && a->message);
    assert(event(&state, CLAUDE("\"hook_event_name\":\"PostToolUse\""), 60) == 1);
    assert(a->status == WORKING && !a->message);
    assert(event(&state, CLAUDE("\"hook_event_name\":\"StopFailure\",\"error\":\"authentication_failed\",\"error_details\":\"Log in again\""), 70) == 1);
    assert(a->status == ERROR && text_is(a->message, "Log in again"));
    assert(event(&state, CLAUDE("\"hook_event_name\":\"Stop\""), 80) == 1);
    assert(a->status == ERROR && a->status_since == 70);
    assert(text_is(a->message, "Log in again"));
    assert(event(&state, CLAUDE("\"hook_event_name\":\"PostToolUse\""), 90) == 1);
    assert(a->status == ERROR);
    assert(event(&state, CLAUDE("\"hook_event_name\":\"UserPromptSubmit\""), 100) == 1);
    assert(a->status == WORKING && !a->message);

    /* Child completions and unknown events cannot invent main-session idle. */
    assert(event(&state, CLAUDE("\"hook_event_name\":\"SubagentStop\",\"agent_id\":\"child\""), 110) == 1);
    assert(event(&state, CLAUDE("\"hook_event_name\":\"Stop\",\"agent_id\":\"child\""), 111) == 1);
    assert(event(&state, CLAUDE("\"hook_event_name\":\"FutureEvent\""), 112) == 0);
    assert(a->status == WORKING && a->last_event_at == 100);
    assert(event(&state, CLAUDE("\"hook_event_name\":\"Stop\""), 120) == 1);
    assert(a->status == IDLE && text_is(a->title, "My title"));

    /* A malformed event is rejected before any metadata changes. */
    assert(event(&state, CLAUDE("\"hook_event_name\":\"UserPromptSubmit\",\"cwd\":37"), 121) == -1);
    assert(a->status == IDLE && text_is(a->cwd, "/work"));
    assert(event(&state, "{\"agent_session\":\"one\",\"agent_kind\":\"codex\",\"session_id\":\"c1\",\"hook_event_name\":\"Stop\"}", 122) == -1);
    assert(event(&state, "{\"agent_session\":null,\"agent_kind\":\"claude\",\"session_id\":\"c1\",\"hook_event_name\":\"Stop\"}", 123) == 0);

    assert(event(&state, CLAUDE("\"hook_event_name\":\"SessionEnd\",\"reason\":\"clear\""), 130) == 1);
    assert(a->status == UNKNOWN && a->status_stale);
    assert(event(&state, "{\"agent_session\":\"one\",\"agent_kind\":\"claude\",\"session_id\":\"c2\",\"hook_event_name\":\"SessionStart\"}", 140) == 1);
    assert(a->status == IDLE && !a->transcript_path);
    assert(text_is(a->conversation_id, "c2") && text_is(a->title, "My title"));
    assert(event(&state, CLAUDE("\"hook_event_name\":\"StopFailure\",\"error\":\"late\""), 150) == 0);
    assert(a->status == IDLE && a->last_event_at == 140);

    assert(event(&state, CODEX("\"hook_event_name\":\"SessionStart\""), 160) == 1);
    assert(event(&state, CODEX("\"hook_event_name\":\"UserPromptSubmit\""), 170) == 1);
    assert(event(&state, CODEX("\"hook_event_name\":\"PermissionRequest\""), 180) == 1);
    assert(b->status == WAITING);
    assert(event(&state, CODEX("\"hook_event_name\":\"PostToolUse\""), 190) == 1);
    assert(b->status == WORKING);
    assert(event(&state, CODEX("\"hook_event_name\":\"Interrupt\""), 200) == 1);
    assert(b->status == UNKNOWN && b->status_stale);
    assert(event(&state, CODEX("\"hook_event_name\":\"Stop\""), 210) == 1);
    assert(b->status == IDLE);
    assert(event(&state, CODEX("\"hook_event_name\":\"StopFailure\""), 220) == 0);

    /* Independent actors: a child's permission survives unrelated parent and
       sibling activity, and all active children must finish before idle. */
    assert(event(&state, CODEX("\"hook_event_name\":\"UserPromptSubmit\",\"turn_id\":\"p2\",\"event_time\":1000"), 221) == 1);
    assert(event(&state, CODEX("\"hook_event_name\":\"SubagentStart\",\"agent_id\":\"worker-a\",\"turn_id\":\"a1\",\"event_time\":1001"), 222) == 1);
    assert(event(&state, CODEX("\"hook_event_name\":\"SubagentStart\",\"agent_id\":\"worker-b\",\"turn_id\":\"b1\",\"event_time\":1002"), 223) == 1);
    assert(event(&state, CODEX("\"hook_event_name\":\"PermissionRequest\",\"agent_id\":\"worker-a\",\"turn_id\":\"a1\",\"event_time\":1003"), 224) == 1);
    assert(event(&state, CODEX("\"hook_event_name\":\"Stop\",\"turn_id\":\"p2\",\"event_time\":1004"), 225) == 1);
    cJSON *aggregate = session_json(b);
    assert(text_is(json_string(aggregate, "status"), "waiting"));
    assert(text_is(json_string(aggregate, "own_status"), "idle"));
    assert(text_is(json_string(aggregate, "attention_agent"), "worker-a"));
    cJSON_Delete(aggregate);
    cJSON *checkpoint = state_json(&state);
    struct state recovered = {0};
    assert(state_decode(&recovered, checkpoint));
    cJSON_Delete(checkpoint);
    aggregate = session_json(session_find(&recovered, "two"));
    assert(text_is(json_string(aggregate, "status"), "waiting"));
    cJSON_Delete(aggregate);
    state_free(&recovered);
    assert(event(&state, CODEX("\"hook_event_name\":\"SubagentStop\",\"agent_id\":\"worker-a\",\"turn_id\":\"a1\",\"event_time\":1005"), 226) == 1);
    aggregate = session_json(b);
    assert(text_is(json_string(aggregate, "status"), "working"));
    cJSON_Delete(aggregate);
    assert(event(&state, CODEX("\"hook_event_name\":\"PermissionRequest\",\"agent_id\":\"worker-a\",\"turn_id\":\"a1\",\"event_time\":1003"), 227) == 0);
    assert(event(&state, CODEX("\"hook_event_name\":\"SubagentStop\",\"agent_id\":\"worker-b\",\"turn_id\":\"b1\",\"event_time\":1006"), 228) == 1);
    aggregate = session_json(b);
    assert(text_is(json_string(aggregate, "status"), "idle"));
    cJSON_Delete(aggregate);
    /* A delayed previous-conversation start or previous-turn prompt cannot
       roll the parent back. A genuinely newer resume remains allowed. */
    assert(event(&state, CODEX("\"hook_event_name\":\"UserPromptSubmit\",\"turn_id\":\"p3\",\"event_time\":2000"), 229) == 1);
    assert(event(&state, "{\"agent_session\":\"two\",\"agent_kind\":\"codex\",\"session_id\":\"old\",\"hook_event_name\":\"SessionStart\",\"event_time\":999}", 230) == 0);
    assert(text_is(b->conversation_id, "x1") && text_is(b->turn_id, "p3"));
    assert(event(&state, CODEX("\"hook_event_name\":\"UserPromptSubmit\",\"turn_id\":\"p2\",\"event_time\":1000"), 231) == 0);
    assert(event(&state, "{\"agent_session\":\"two\",\"agent_kind\":\"codex\",\"session_id\":\"old\",\"hook_event_name\":\"PermissionRequest\",\"agent_id\":\"orphan\",\"event_time\":3000}", 232) == 0);
    aggregate = session_json(b);
    assert(cJSON_GetArraySize(cJSON_GetObjectItemCaseSensitive(aggregate, "children")) == 2);
    cJSON_Delete(aggregate);

    cJSON *saved = state_json(&state);
    struct state loaded = {0};
    assert(state_decode(&loaded, saved));
    assert(loaded.count == 2);
    struct session *restored = session_find(&loaded, "one");
    assert(restored && text_is(restored->title, "My title"));
    assert(restored->status == IDLE && restored->status_since == 140);
    cJSON_ReplaceItemInObject(saved, "version", cJSON_CreateNumber(999));
    assert(!state_decode(&loaded, saved) && loaded.count == 2);
    cJSON_Delete(saved);
    session_remove(&state, "one");
    assert(!session_find(&state, "one") && state.count == 1);
    assert(event(&state, CLAUDE("\"hook_event_name\":\"SessionStart\""), 230) == 0);
    state_free(&loaded);
    state_free(&state);
    puts("PASS: session transitions, isolation, error retention, conversation changes, snapshot validation");
    return 0;
}
