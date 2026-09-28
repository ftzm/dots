#include "session.h"

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

/* Read only the path supplied by the harness, never search the user's history.
   One bounded read per poll; partial records wait for their terminating newline.
   Replay after restart is safe because terminal records must match turn_id. */
#define READ_BUDGET 65536
#define LINE_LIMIT (1024 * 1024)
struct transcript {
    dev_t device;
    ino_t inode;
    off_t offset;
    char *line, *conversation, *prompt;
    size_t used, capacity;
    bool skipping;
    bool catching_up;
    const char *problem;
    char *terminal_turn, *terminal_message;
    const char *terminal_event;
    enum agent_status terminal_status;
    bool terminal_applied;
};

bool transcript_pending(const struct session *s)
{
    return s->transcript && s->transcript->catching_up;
}

void transcript_reset(struct session *s)
{
    struct transcript *r = s->transcript;
    if (!r) return;
    free(r->line); free(r->conversation); free(r->prompt);
    free(r->terminal_turn); free(r->terminal_message); free(r);
    s->transcript = NULL;
}

static bool health(struct session *s, const char *error)
{
    bool stale_changed = error && !s->status_stale;
    if (error) s->status_stale = true;
    if ((!error && !s->observation_error) || text_is(error, s->observation_error)) return stale_changed;
    replace_string(&s->observation_error, error);
    if (error) {
        fprintf(stderr, "agentd: session %s: %s\n", s->id, error);
    }
    return true;
}

static void terminal(struct transcript *r, const char *turn, enum agent_status status,
                     const char *message, const char *event)
{
    if (!turn || !*turn || strlen(turn) > 8192) return;
    replace_string(&r->terminal_turn, turn);
    /* Never retain whole responses or unbounded API error bodies. */
    char bounded[8193];
    if (message) { snprintf(bounded, sizeof(bounded), "%s", message); message = bounded; }
    replace_string(&r->terminal_message, message);
    r->terminal_status = status;
    r->terminal_event = event;
    r->terminal_applied = false;
}

static void codex_record(struct session *s, struct transcript *r, const cJSON *record)
{
    const cJSON *payload = cJSON_GetObjectItemCaseSensitive(record, "payload");
    if (text_is(json_string(record, "type"), "session_meta")) {
        replace_string(&r->conversation, json_string(payload, "id"));
        if (!text_is(r->conversation, s->conversation_id)) r->problem = "Transcript conversation does not match session";
    }
    if (!text_is(r->conversation, s->conversation_id) || !text_is(json_string(record, "type"), "event_msg")) return;
    const char *type = json_string(payload, "type"), *turn = json_string(payload, "turn_id");
    if (text_is(type, "task_complete")) {
        const cJSON *error = cJSON_GetObjectItemCaseSensitive(payload, "error");
        if (error && !cJSON_IsNull(error)) {
            const char *message = json_string(error, "message");
            terminal(r, turn, ERROR, message ? message : "Codex turn failed", "TranscriptFailure");
        } else terminal(r, turn, IDLE, NULL, "TranscriptComplete");
    } else if (text_is(type, "turn_aborted")) {
        if (text_is(json_string(payload, "reason"), "interrupted"))
            terminal(r, turn, IDLE, "Cancelled by user", "TranscriptInterrupted");
        else terminal(r, turn, UNKNOWN, "Turn aborted; readiness unknown", "TranscriptAborted");
    }
}

static void claude_record(struct session *s, struct transcript *r, const cJSON *record)
{
    if (!text_is(json_string(record, "sessionId"), s->conversation_id)
        || (s->child ? (!cJSON_IsTrue(cJSON_GetObjectItemCaseSensitive(record, "isSidechain"))
                        || !text_is(json_string(record, "agentId"), s->id))
                     : !cJSON_IsFalse(cJSON_GetObjectItemCaseSensitive(record, "isSidechain")))) return;
    const char *type = json_string(record, "type");
    const char *prompt = json_string(record, "promptId");
    if (text_is(type, "user") && prompt && strlen(prompt) <= 8192) replace_string(&r->prompt, prompt);
    const cJSON *message = cJSON_GetObjectItemCaseSensitive(record, "message");
    const cJSON *content = cJSON_GetObjectItemCaseSensitive(message, "content");
    const cJSON *first = cJSON_GetArrayItem(content, 0);
    const char *text = json_string(first, "text");
    if (text_is(type, "assistant") && cJSON_IsTrue(cJSON_GetObjectItemCaseSensitive(record, "isApiErrorMessage")))
        terminal(r, prompt ? prompt : r->prompt, ERROR, text ? text : "Claude turn failed", "TranscriptFailure");
    else if (text_is(type, "user") && !cJSON_GetObjectItemCaseSensitive(record, "promptSource")
             && cJSON_IsArray(content) && cJSON_GetArraySize(content) == 1 && text_is(json_string(first, "type"), "text")
             && (text_is(text, "[Request interrupted by user for tool use]")
                 || text_is(text, "[Request interrupted by user]")))
        terminal(r, prompt, IDLE, "Cancelled by user", "TranscriptInterrupted");
}

static void record(struct session *s, struct transcript *r)
{
    r->line[r->used] = '\0';
    cJSON *value = json_parse(r->line, r->used);
    if (!cJSON_IsObject(value)) r->problem = "Cannot parse transcript record; status coverage is incomplete";
    else if (text_is(s->kind, "codex")) codex_record(s, r, value);
    else if (text_is(s->kind, "claude")) claude_record(s, r, value);
    cJSON_Delete(value);
}

bool transcript_poll(struct session *s, int64_t now)
{
    if (!s->conversation_id) return false;
    if (!s->turn_id || !*s->turn_id) {
        if (s->status == WORKING || s->status == WAITING || s->status == ERROR)
            return health(s, "Active turn ID unavailable; failure/cancellation coverage is incomplete");
        return false;
    }
    if (!s->transcript_path || s->transcript_path[0] != '/')
        return health(s, "Transcript unavailable; failure/cancellation coverage is incomplete");
    int fd = open(s->transcript_path, O_RDONLY | O_CLOEXEC | O_NONBLOCK | O_NOFOLLOW);
    if (fd < 0) return health(s, "Cannot open transcript; failure/cancellation coverage is incomplete");
    struct stat st;
    if (fstat(fd, &st) < 0 || !S_ISREG(st.st_mode) || st.st_uid != geteuid()) {
        close(fd);
        return health(s, "Transcript must be a regular file owned by this user");
    }
    struct transcript *r = s->transcript;
    if (r && (r->device != st.st_dev || r->inode != st.st_ino || st.st_size < r->offset)) {
        transcript_reset(s);
        r = NULL;
    }
    if (!r) {
        r = s->transcript = allocate(sizeof(*r));
        r->device = st.st_dev;
        r->inode = st.st_ino;
    }
    char bytes[READ_BUDGET];
    ssize_t count = pread(fd, bytes, sizeof(bytes), r->offset);
    close(fd);
    if (count < 0) return health(s, "Cannot read transcript; failure/cancellation coverage is incomplete");
    r->offset += count;
    r->catching_up = r->offset < st.st_size;
    for (ssize_t i = 0; i < count; i++) {
        char byte = bytes[i];
        if (byte == '\n') {
            if (!r->skipping && r->used) record(s, r);
            r->used = 0;
            r->skipping = false;
        } else if (!r->skipping) {
            if (r->used == LINE_LIMIT) {
                r->skipping = true;
                r->problem = "Oversized transcript record skipped; status coverage is incomplete";
                continue;
            }
            if (r->used + 1 >= r->capacity) {
                size_t capacity = r->capacity ? r->capacity * 2 : 4096;
                char *line = allocate(capacity);
                if (r->used) memcpy(line, r->line, r->used);
                free(r->line); r->line = line; r->capacity = capacity;
            }
            r->line[r->used++] = byte;
        }
    }
    bool changed = false;
    if (r->offset == st.st_size && st.st_size > 0 && !r->used && text_is(s->kind, "codex") && !r->conversation)
        r->problem = "Codex transcript has no recognized session metadata; status coverage is incomplete";
    /* Apply only after catching up: replay must not briefly resurrect an old
       failure while a later record from the same read already supersedes it. */
    if (r->offset == st.st_size && r->terminal_turn && !r->terminal_applied) {
        changed = session_terminal(s, r->terminal_turn, r->terminal_status,
                                   r->terminal_message, r->terminal_event, now);
        if (text_is(r->terminal_turn, s->turn_id)) r->terminal_applied = true;
        if (changed && r->terminal_status == UNKNOWN) s->status_stale = true;
    }
    return health(s, r->problem) || changed;
}
