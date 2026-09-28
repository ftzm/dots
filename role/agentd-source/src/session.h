#ifndef AGENTD_SESSION_H
#define AGENTD_SESSION_H

#include "cJSON.h"
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

enum agent_status { UNKNOWN, WORKING, WAITING, IDLE, ERROR, EXITED };
enum zmx_status { ZMX_UNKNOWN, ZMX_ALIVE, ZMX_DEAD };

struct session {
    char *id, *title, *kind, *cwd, *conversation_id, *transcript_path;
    char *last_event, *message, *notification_type;
    char *turn_id, *observation_error;
    bool turn_finished;
    bool managed, launch_pending, exit_expected, kill_requested;
    int exit_code; /* -1 until a wrapper exit receipt is read. */
    int64_t launch_deadline, lifecycle_revision;
    char *lifecycle_error;
    struct transcript *transcript; /* Runtime reader; rebuilt from the file after restart. */
    enum agent_status status;
    enum zmx_status zmx_state;
    bool status_stale;
    int64_t last_event_at, status_since;
    struct session *children; /* Independent child turns, linked by next. */
    bool child;
    int64_t event_time; /* Forwarder timestamp; reject older deliveries per actor. */
    struct session *next;
};

struct state { struct session *sessions; size_t count; };

/* Allocations fail the process, allowing supervision to restart it. */
void *allocate(size_t size);
char *copy_string(const char *text);
void replace_string(char **dest, const char *text);
const char *json_string(const cJSON *object, const char *key);
bool text_is(const char *a, const char *b);
bool valid_id(const char *id);
bool valid_kind(const char *kind);
void json_put(cJSON *object, const char *key, cJSON *value);
void json_append(cJSON *array, cJSON *value);
char *json_print(const cJSON *value);
/* text[length] must be NUL. Reject NULs that cJSON's C strings cannot retain. */
cJSON *json_parse(const char *text, size_t length);

struct session *session_find(struct state *state, const char *id);
struct session *session_add(struct state *state, const char *id, const char *kind,
                            const char *cwd, const char *title, int64_t now);
void session_remove(struct state *state, const char *id);
void state_free(struct state *state);
cJSON *session_json(const struct session *session);
cJSON *state_json(const struct state *state);
/* Validate the whole snapshot before replacing state. */
bool state_decode(struct state *state, const cJSON *root);
/* -1 invalid, 0 ignored, 1 applied; hooks never create sessions. */
int session_event(struct state *state, const cJSON *event, int64_t now,
                  struct session **changed);

struct transition {
    enum agent_status status;
    bool informative, stale, recovery;
    const char *message, *notification_type;
};
bool claude_event(const cJSON *event, struct transition *transition);
bool codex_event(const cJSON *event, struct transition *transition);

/* Supplemental terminal records; only the current conversation/turn may apply. */
bool session_terminal(struct session *s, const char *turn, enum agent_status status,
                      const char *message, const char *event, int64_t now);
bool session_has_error(const struct session *s);
bool session_pending(const struct session *s);
bool session_poll(struct session *s, int64_t now);
bool transcript_poll(struct session *s, int64_t now);
void transcript_reset(struct session *s);
bool transcript_pending(const struct session *s);

#endif
