#include "session.h"

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static const char *statuses[] = {"unknown", "working", "waiting", "idle", "error", "exited"};
static const char *liveness[] = {"unknown", "alive", "dead"};

static void out_of_memory(void)
{
    fputs("agentd: out of memory\n", stderr);
    exit(1);
}

void *allocate(size_t size)
{
    void *result = calloc(1, size ? size : 1);
    if (!result) out_of_memory();
    return result;
}

char *copy_string(const char *text)
{
    if (!text) return NULL;
    char *result = allocate(strlen(text) + 1);
    strcpy(result, text);
    return result;
}

void replace_string(char **dest, const char *text)
{
    char *copy = copy_string(text);
    free(*dest);
    *dest = copy;
}

const char *json_string(const cJSON *object, const char *key)
{
    return cJSON_GetStringValue(cJSON_GetObjectItemCaseSensitive(object, key));
}

bool text_is(const char *a, const char *b)
{
    return a && b && strcmp(a, b) == 0;
}

bool valid_id(const char *id)
{
    if (!id || !*id || strlen(id) > 128) return false;
    for (const unsigned char *p = (const unsigned char *)id; *p; p++)
        if (!isalnum(*p) && *p != '-' && *p != '_') return false;
    return true;
}

bool valid_kind(const char *kind)
{
    return text_is(kind, "claude") || text_is(kind, "codex") || text_is(kind, "unknown");
}

void json_put(cJSON *object, const char *key, cJSON *value)
{
    if (!object || !value || !cJSON_AddItemToObject(object, key, value)) out_of_memory();
}

void json_append(cJSON *array, cJSON *value)
{
    if (!array || !value || !cJSON_AddItemToArray(array, value)) out_of_memory();
}

char *json_print(const cJSON *value)
{
    char *text = cJSON_PrintUnformatted(value);
    if (!text) out_of_memory();
    return text;
}

cJSON *json_parse(const char *text, size_t length)
{
    if (memchr(text, '\0', length)) return NULL;
    for (size_t i = 0; i < length; i++) {
        if (text[i] == '\\' && i + 1 < length) {
            i++;
            if (text[i] == 'u' && length - i > 4 && memcmp(text + i + 1, "0000", 4) == 0)
                return NULL;
        }
    }
    return cJSON_ParseWithLengthOpts(text, length + 1, NULL, true);
}

struct session *session_find(struct state *state, const char *id)
{
    for (struct session *s = state->sessions; s; s = s->next)
        if (text_is(s->id, id)) return s;
    return NULL;
}

struct session *session_add(struct state *state, const char *id, const char *kind,
                            const char *cwd, const char *title, int64_t now)
{
    if (!valid_id(id) || !valid_kind(kind) || state->count >= 256 || session_find(state, id))
        return NULL;
    struct session *s = allocate(sizeof(*s));
    s->exit_code = -1;
    s->id = copy_string(id);
    s->kind = copy_string(kind);
    s->cwd = copy_string(cwd);
    s->title = copy_string(title);
    s->status_stale = true;
    s->status_since = now;
    s->next = state->sessions;
    state->sessions = s;
    state->count++;
    return s;
}

static void session_free(struct session *s)
{
    transcript_reset(s);
    while (s->children) {
        struct session *child = s->children;
        s->children = child->next;
        session_free(child);
    }
    free(s->id); free(s->title); free(s->kind); free(s->cwd);
    free(s->conversation_id); free(s->transcript_path); free(s->last_event);
    free(s->message); free(s->notification_type);
    free(s->turn_id); free(s->observation_error); free(s->lifecycle_error); free(s);
}

void session_remove(struct state *state, const char *id)
{
    for (struct session **p = &state->sessions; *p; p = &(*p)->next) {
        if (text_is((*p)->id, id)) {
            struct session *s = *p;
            *p = s->next;
            session_free(s);
            state->count--;
            return;
        }
    }
}

void state_free(struct state *state)
{
    while (state->sessions) session_remove(state, state->sessions->id);
}

static void put_text(cJSON *object, const char *key, const char *value)
{
    json_put(object, key, value ? cJSON_CreateString(value) : cJSON_CreateNull());
}

/* A child never replaces the parent's lifecycle. Aggregate only for clients.
   Errors/waits win, then active work, then the parent's terminal state. */
static const struct session *effective(const struct session *s)
{
    const struct session *best = s;

    const int rank[] = {1, 2, 3, 0, 4, 0};
    for (const struct session *c = s->children; c; c = c->next)
        if ((s->status != EXITED && s->zmx_state != ZMX_DEAD) || c->status == ERROR)
            if (rank[c->status] > rank[best->status]) best = c;
    return best;
}

cJSON *session_json(const struct session *s)
{
    cJSON *object = cJSON_CreateObject();
#define TEXT(field) put_text(object, #field, s->field)
    TEXT(id); TEXT(title); TEXT(kind); TEXT(cwd); TEXT(conversation_id);
    TEXT(transcript_path); TEXT(last_event); TEXT(message); TEXT(notification_type);
    TEXT(turn_id); TEXT(observation_error);
    TEXT(lifecycle_error);
#undef TEXT
    const struct session *display = effective(s);
    put_text(object, "status", statuses[display->status]);
    put_text(object, "own_status", statuses[s->status]);
    put_text(object, "attention_agent", display == s ? NULL : display->id);
    put_text(object, "own_message", s->message);
    put_text(object, "own_notification_type", s->notification_type);
    json_put(object, "own_status_stale", cJSON_CreateBool(s->status_stale));
    json_put(object, "event_time", cJSON_CreateNumber((double)s->event_time));
    if (display != s) {
        cJSON_ReplaceItemInObjectCaseSensitive(object, "message", display->message ? cJSON_CreateString(display->message) : cJSON_CreateNull());
        cJSON_ReplaceItemInObjectCaseSensitive(object, "notification_type", display->notification_type ? cJSON_CreateString(display->notification_type) : cJSON_CreateNull());
    }
    cJSON *children = cJSON_CreateArray();
    for (const struct session *c = s->children; c; c = c->next) json_append(children, session_json(c));
    json_put(object, "children", children);
    put_text(object, "zmx_state", liveness[s->zmx_state]);
    json_put(object, "status_stale", cJSON_CreateBool(display->status_stale));
    json_put(object, "turn_finished", cJSON_CreateBool(s->turn_finished));
    json_put(object, "managed", cJSON_CreateBool(s->managed));
    json_put(object, "launch_pending", cJSON_CreateBool(s->launch_pending));
    json_put(object, "exit_expected", cJSON_CreateBool(s->exit_expected));
    json_put(object, "kill_requested", cJSON_CreateBool(s->kill_requested));
    json_put(object, "exit_code", s->exit_code < 0 ? cJSON_CreateNull() : cJSON_CreateNumber(s->exit_code));
    json_put(object, "launch_deadline", cJSON_CreateNumber((double)s->launch_deadline));
    json_put(object, "status_since", cJSON_CreateNumber((double)display->status_since));
    json_put(object, "own_status_since", cJSON_CreateNumber((double)s->status_since));
    json_put(object, "last_event_at", s->last_event_at ? cJSON_CreateNumber((double)s->last_event_at) : cJSON_CreateNull());
    return object;
}

cJSON *state_json(const struct state *state)
{
    cJSON *root = cJSON_CreateObject(), *sessions = cJSON_CreateArray();
    json_put(root, "version", cJSON_CreateNumber(1));
    json_put(root, "sessions", sessions);
    for (const struct session *s = state->sessions; s; s = s->next)
        json_append(sessions, session_json(s));
    return root;
}

static bool optional_text(const cJSON *object, const char *key)
{
    const cJSON *value = cJSON_GetObjectItemCaseSensitive(object, key);
    return !value || cJSON_IsNull(value) || (cJSON_IsString(value) && strlen(value->valuestring) <= 8192);
}

static int enum_index(const char *value, const char **names, size_t count)
{
    for (size_t i = 0; i < count; i++)
        if (text_is(value, names[i])) return (int)i;
    return -1;
}

static bool timestamp(const cJSON *value)
{
    return cJSON_IsNumber(value) && value->valuedouble >= 0 && value->valuedouble <= 9007199254740991.0
        && value->valuedouble == (double)(int64_t)value->valuedouble;
}

static bool decode(struct state *state, const cJSON *root, bool child)
{
    const cJSON *version = cJSON_GetObjectItemCaseSensitive(root, "version");
    const cJSON *sessions = cJSON_GetObjectItemCaseSensitive(root, "sessions");
    if (!cJSON_IsObject(root) || !cJSON_IsNumber(version) || version->valuedouble != 1 || !cJSON_IsArray(sessions))
        return false;
    struct state decoded = {0};
    const cJSON *item;
    cJSON_ArrayForEach(item, sessions) {
        const char *fields[] = {"title", "cwd", "conversation_id", "transcript_path", "last_event", "message", "notification_type", "turn_id", "observation_error", "lifecycle_error"};
        if (!cJSON_IsObject(item)) goto invalid;
        for (size_t i = 0; i < sizeof(fields) / sizeof(fields[0]); i++)
            if (!optional_text(item, fields[i])) goto invalid;
        int status = enum_index(json_string(item, cJSON_HasObjectItem(item, "own_status") ? "own_status" : "status"), statuses, 6);
        int zmx = enum_index(json_string(item, "zmx_state"), liveness, 3);
        const cJSON *stale = cJSON_GetObjectItemCaseSensitive(item, cJSON_HasObjectItem(item, "own_status_stale") ? "own_status_stale" : "status_stale");
        const cJSON *since = cJSON_GetObjectItemCaseSensitive(item, cJSON_HasObjectItem(item, "own_status_since") ? "own_status_since" : "status_since");
        const cJSON *last = cJSON_GetObjectItemCaseSensitive(item, "last_event_at");
        const cJSON *finished = cJSON_GetObjectItemCaseSensitive(item, "turn_finished");
        if (finished && !cJSON_IsBool(finished)) goto invalid;
        const char *flags[] = {"managed", "launch_pending", "exit_expected", "kill_requested"};
        for (size_t f = 0; f < sizeof(flags)/sizeof(flags[0]); f++) {
            const cJSON *v = cJSON_GetObjectItemCaseSensitive(item, flags[f]);
            if (v && !cJSON_IsBool(v)) goto invalid;
        }
        const cJSON *deadline = cJSON_GetObjectItemCaseSensitive(item, "launch_deadline");
        const cJSON *code = cJSON_GetObjectItemCaseSensitive(item, "exit_code");
        if (deadline && !timestamp(deadline)) goto invalid;
        if (code && !cJSON_IsNull(code) && (!timestamp(code) || code->valuedouble > 255)) goto invalid;
        if (status < 0 || zmx < 0 || !cJSON_IsBool(stale) || !timestamp(since)
            || (!cJSON_IsNull(last) && !timestamp(last))) goto invalid;
        struct session *s = session_add(&decoded, json_string(item, "id"), json_string(item, "kind"),
                                        json_string(item, "cwd"), json_string(item, "title"), (int64_t)since->valuedouble);
        if (!s) goto invalid;
        const cJSON *clock = cJSON_GetObjectItemCaseSensitive(item, "event_time");
        if (clock && !timestamp(clock)) goto invalid;
        s->event_time = clock ? (int64_t)clock->valuedouble : 0;
        s->child = child;
        const cJSON *children = cJSON_GetObjectItemCaseSensitive(item, "children");
        if (children && (!cJSON_IsArray(children) || (child && cJSON_GetArraySize(children)))) goto invalid;
        if (children && cJSON_GetArraySize(children)) {
            cJSON *root = cJSON_CreateObject();
            json_put(root, "version", cJSON_CreateNumber(1));
            json_put(root, "sessions", cJSON_Duplicate(children, true));
            struct state nested = {0};
            bool ok = decode(&nested, root, true);
            cJSON_Delete(root);
            if (!ok) goto invalid;
            s->children = nested.sessions;
        }
        s->status = status;
        s->zmx_state = zmx;
        s->status_stale = cJSON_IsTrue(stale);
        s->turn_finished = cJSON_IsTrue(finished);
        s->managed = cJSON_IsTrue(cJSON_GetObjectItemCaseSensitive(item, "managed"));
        s->launch_pending = cJSON_IsTrue(cJSON_GetObjectItemCaseSensitive(item, "launch_pending"));
        s->exit_expected = cJSON_IsTrue(cJSON_GetObjectItemCaseSensitive(item, "exit_expected"));
        s->kill_requested = cJSON_IsTrue(cJSON_GetObjectItemCaseSensitive(item, "kill_requested"));
        s->launch_deadline = deadline ? (int64_t)deadline->valuedouble : 0;
        s->exit_code = cJSON_IsNumber(code) ? (int)code->valuedouble : -1;
        s->last_event_at = cJSON_IsNumber(last) ? (int64_t)last->valuedouble : 0;
#define COPY(field) s->field = copy_string(json_string(item, #field))
        COPY(conversation_id); COPY(transcript_path); COPY(last_event); COPY(message); COPY(notification_type);
        COPY(turn_id); COPY(observation_error);
        COPY(lifecycle_error);
#undef COPY
        if (cJSON_HasObjectItem(item, "own_message")) {
            if (!optional_text(item, "own_message") || !optional_text(item, "own_notification_type")) goto invalid;
            replace_string(&s->message, json_string(item, "own_message"));
            replace_string(&s->notification_type, json_string(item, "own_notification_type"));
        }
    }
    state_free(state);
    *state = decoded;
    return true;
invalid:
    state_free(&decoded);
    return false;
}

bool state_decode(struct state *state, const cJSON *root)
{
    return decode(state, root, false);
}

static int child_event(struct session *s, const cJSON *event, int64_t now)
{
    const char *id = json_string(event, "agent_id");
    const char *name = json_string(event, "hook_event_name");
    if (!valid_id(id)) return -1;
    struct transition probe = {0};
    if (!text_is(name, "SubagentStart") && !text_is(name, "SubagentStop")
        && !(text_is(s->kind, "claude") ? claude_event(event, &probe) : codex_event(event, &probe))) return 0;
    /* Both native hook envelopes identify the root conversation. Codex's
       child transcript uses a different ID, translated below. */
    if (s->conversation_id && !text_is(s->conversation_id, json_string(event, "session_id"))) return 0;
    struct state children = {.sessions = s->children};
    for (struct session *c = children.sessions; c; c = c->next) children.count++;
    struct session *c = session_find(&children, id);
    if (!c) {
        c = session_add(&children, id, s->kind, s->cwd, NULL, now);
        if (!c) {
            s->status = ERROR; s->status_stale = false; s->status_since = now;
            replace_string(&s->message, "Child state limit reached; start a new conversation to restore coverage");
            return 1;
        }
        c->child = true;
        s->children = children.sessions;
    }
    cJSON *copy = cJSON_Duplicate(event, true);
    cJSON_DeleteItemFromObjectCaseSensitive(copy, "agent_id");
    cJSON_ReplaceItemInObjectCaseSensitive(copy, "agent_session", cJSON_CreateString(id));
    if (text_is(name, "SubagentStart"))
        cJSON_ReplaceItemInObjectCaseSensitive(copy, "hook_event_name", cJSON_CreateString("UserPromptSubmit"));
    if (text_is(name, "SubagentStop"))
        cJSON_ReplaceItemInObjectCaseSensitive(copy, "hook_event_name", cJSON_CreateString("Stop"));
    /* Claude's ordinary child hooks name the MAIN transcript. Never tail that
       file as child evidence; SubagentStop supplies the actual child path. */
    if (text_is(s->kind, "codex")) {
        /* Native hooks associate session_id with the root, whereas the saved
           child's session_meta.id is agent_id. SubagentStop names both files. */
        cJSON_ReplaceItemInObjectCaseSensitive(copy, "session_id", cJSON_CreateString(id));
        const char *path = json_string(event, "agent_transcript_path");
        if (path) {
            cJSON_DeleteItemFromObjectCaseSensitive(copy, "transcript_path");
            json_put(copy, "transcript_path", cJSON_CreateString(path));
        }
    }
    if (text_is(s->kind, "claude")) {
        /* Native background hooks read the parent's CURRENT prompt_id, which
           changes while this child is still running. Keep the child's launch
           prompt for transcript matching; per-child event_time orders delivery. */
        if (c->turn_id && !text_is(name, "SubagentStart") && !text_is(name, "UserPromptSubmit")) {
            cJSON_DeleteItemFromObjectCaseSensitive(copy, "prompt_id");
            json_put(copy, "prompt_id", cJSON_CreateString(c->turn_id));
        }
        cJSON_DeleteItemFromObjectCaseSensitive(copy, "transcript_path");
        const char *path = json_string(event, "agent_transcript_path");
        if (path) json_put(copy, "transcript_path", cJSON_CreateString(path));
    }
    struct session *changed;
    int result = session_event(&children, copy, now, &changed);
    cJSON_Delete(copy);
    return result;
}

bool session_has_error(const struct session *s)
{
    return effective(s)->status == ERROR;
}

bool session_pending(const struct session *s)
{
    if (transcript_pending(s)) return true;
    for (const struct session *c = s->children; c; c = c->next)
        if (transcript_pending(c)) return true;
    return false;
}

bool session_poll(struct session *s, int64_t now)
{
    bool changed = transcript_poll(s, now);
    for (struct session *c = s->children; c; c = c->next)
        if (c->transcript_path) changed |= transcript_poll(c, now);
    return changed;
}

int session_event(struct state *state, const cJSON *event, int64_t now,
                  struct session **changed)
{
    *changed = NULL;
    const char *fields[] = {"agent_session", "agent_kind", "hook_event_name", "session_id", "cwd",
        "transcript_path", "agent_transcript_path", "reason", "source", "agent_id", "message", "notification_type", "tool_name", "error", "error_details", "prompt_id", "turn_id"};
    if (!cJSON_IsObject(event)) return -1;
    for (size_t i = 0; i < sizeof(fields) / sizeof(fields[0]); i++)
        if (!optional_text(event, fields[i])) return -1;
    if (text_is(json_string(event, "hook_event_name"), "StopFailure")
        && !optional_text(event, "last_assistant_message")) return -1;
    const char *id = json_string(event, "agent_session");
    const char *kind = json_string(event, "agent_kind");
    const char *name = json_string(event, "hook_event_name");
    const char *conversation = json_string(event, "session_id");
    if (!valid_kind(kind) || !name || !*name || !conversation || !*conversation) return -1;
    const cJSON *clock = cJSON_GetObjectItemCaseSensitive(event, "event_time");
    if (clock && !timestamp(clock)) return -1;
    if (!id) return 0; /* Unassociated events are not sessions. */
    if (!valid_id(id)) return -1;
    struct session *s = session_find(state, id);
    if (!s) return 0;
    if (!text_is(s->kind, kind) && !text_is(s->kind, "unknown")) return -1;
    if (s->managed && (s->exit_code >= 0 || s->zmx_state == ZMX_DEAD)
        && !text_is(name, "StopFailure"))
        return 0; /* Delayed hooks cannot revive a process with a known exit. */
    if (json_string(event, "agent_id") || text_is(name, "SubagentStart") || text_is(name, "SubagentStop")) {
        int result = child_event(s, event, now);
        if (result == 1) *changed = s;
        return result;
    }
    int64_t event_time = clock ? (int64_t)clock->valuedouble : 0;
    if (event_time && event_time < s->event_time) return 0;
    bool starting = text_is(name, "SessionStart");
    bool prompting = text_is(name, "UserPromptSubmit");
    const char *turn = json_string(event, text_is(kind, "claude") ? "prompt_id" : "turn_id");
    bool switching = s->conversation_id && !text_is(s->conversation_id, conversation);
    if (switching && !starting) return 0;
    if (!starting && !prompting && !text_is(name, "SessionEnd")
        && turn && s->turn_id && !text_is(turn, s->turn_id)) return 0;
    struct transition transition = {0};
    bool known = text_is(kind, "claude") ? claude_event(event, &transition)
                : text_is(kind, "codex") && codex_event(event, &transition);
    if (!known) return 0;
    if (starting && s->conversation_id && !switching && s->status != EXITED && s->status != UNKNOWN)
        transition.informative = false; /* Duplicate startup must not reset an active turn. */
    if (switching) {
        while (s->children) {
            struct session *child = s->children;
            s->children = child->next;
            session_free(child);
        }
        transcript_reset(s);
        replace_string(&s->transcript_path, NULL);
        replace_string(&s->turn_id, NULL);
        s->turn_finished = false;
        transition.recovery = true;
    }
    if (prompting && (!turn || !text_is(turn, s->turn_id))) {
        replace_string(&s->turn_id, turn);
        s->turn_finished = false;
    }
    if (!s->child && text_is(kind, "claude") && text_is(name, "Stop")) {
        const cJSON *task;
        cJSON_ArrayForEach(task, cJSON_GetObjectItemCaseSensitive(event, "background_tasks")) {
            const char *child_id = json_string(task, "id");
            if (!text_is(json_string(task, "type"), "subagent") || !text_is(json_string(task, "status"), "running") || !valid_id(child_id)) continue;
            struct state children = {.sessions = s->children};
            for (struct session *c = s->children; c; c = c->next) children.count++;
            if (session_find(&children, child_id)) continue; /* Completion evidence beats a stale Stop snapshot. */
            struct session *c = session_add(&children, child_id, kind, s->cwd, NULL, now);
            if (c) {
                c->child = true; c->status = WORKING;
                c->conversation_id = copy_string(conversation);
                s->children = children.sessions;
            }
        }
    }
    if (s->turn_finished && !starting && !text_is(name, "SessionEnd") && transition.status != ERROR)
        transition.informative = false; /* Delayed tool hooks cannot undo a terminal record. */
    if (event_time) s->event_time = event_time;
    replace_string(&s->kind, kind);
    replace_string(&s->conversation_id, conversation);
    if (cJSON_GetObjectItemCaseSensitive(event, "cwd")) replace_string(&s->cwd, json_string(event, "cwd"));
    if (cJSON_GetObjectItemCaseSensitive(event, "transcript_path")) {
        if (!text_is(s->transcript_path, json_string(event, "transcript_path"))) transcript_reset(s);
        replace_string(&s->transcript_path, json_string(event, "transcript_path"));
    }
    replace_string(&s->last_event, name);
    s->last_event_at = now;
    if (transition.informative && (s->status != ERROR || transition.recovery || transition.status == ERROR)) {
        if (s->status != transition.status) s->status_since = now;
        s->status = transition.status;
        s->status_stale = transition.stale || s->observation_error != NULL;
        replace_string(&s->message, transition.message);
        replace_string(&s->notification_type, transition.notification_type);
    }
    if (text_is(name, "Stop") || text_is(name, "StopFailure")) s->turn_finished = true;
    *changed = s;
    return 1;
}

bool session_terminal(struct session *s, const char *turn, enum agent_status status,
                      const char *message, const char *event, int64_t now)
{
    if (!text_is(turn, s->turn_id)) return false;
    if (status != ERROR && (s->status == ERROR || s->status == EXITED)) return false;
    if (s->turn_finished && s->status == status && !s->status_stale) return false;
    s->turn_finished = true;
    if (s->status != status) s->status_since = now;
    s->status = status;
    s->status_stale = false;
    replace_string(&s->message, message);
    replace_string(&s->notification_type, NULL);
    replace_string(&s->last_event, event);
    s->last_event_at = now;
    return true;
}
