#include "session.h"
#include "store.h"
#include "zmx.h"

#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/file.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/un.h>
#include <time.h>
#include <unistd.h>

#define MAX_PEERS 64
#define MAX_LINE (1024 * 1024)
#define MAX_OUTPUT (4 * 1024 * 1024)
#define INPUT_TIMEOUT_MS 2000

struct peer {
    int fd;
    bool ingest, eof;
    char *input, *output;
    size_t used, queued, sent;
    int64_t incomplete_since;
};

static struct state state;
static struct peer peers[MAX_PEERS];
static const char *state_path;
static bool dirty;
static int64_t retry_at;
static char *zmx_dir, *exit_dir;
static const char *zmx_program = "zmx";
static struct zmx_job zmx_job;
static int64_t probe_at, revision, probe_revision;
static bool probe_next;
static char *kill_cursor;
static char *join(const char *base, const char *suffix);
static volatile sig_atomic_t stopping;

static int64_t milliseconds(clockid_t clock)
{
    struct timespec t;
    if (clock_gettime(clock, &t) < 0) { perror("clock_gettime"); exit(1); }
    return (int64_t)t.tv_sec * 1000 + t.tv_nsec / 1000000;
}

static void stop(int signum) { (void)signum; stopping = 1; }

static void drop(struct peer *p)
{
    if (p->fd >= 0) close(p->fd);
    free(p->input);
    free(p->output);
    *p = (struct peer){.fd = -1};
}

static void queue_text(struct peer *p, const char *text)
{
    if (p->fd < 0) return;
    size_t length = strlen(text), pending = p->queued - p->sent;
    if (length + 1 > MAX_OUTPUT - pending) {
        fputs("agentd: disconnecting client: output queue limit\n", stderr);
        drop(p);
        return;
    }
    char *output = allocate(pending + length + 1);
    if (pending) memcpy(output, p->output + p->sent, pending);
    memcpy(output + pending, text, length);
    output[pending + length] = '\n';
    free(p->output);
    p->output = output;
    p->sent = 0;
    p->queued = pending + length + 1;
}

static void send_json(struct peer *p, cJSON *value)
{
    char *text = json_print(value);
    queue_text(p, text);
    free(text);
    cJSON_Delete(value);
}

static void broadcast(cJSON *value)
{
    char *text = json_print(value);
    for (size_t i = 0; i < MAX_PEERS; i++)
        if (peers[i].fd >= 0 && !peers[i].ingest) queue_text(&peers[i], text);
    free(text);
    cJSON_Delete(value);
}

static cJSON *message(const char *type)
{
    cJSON *value = cJSON_CreateObject();
    json_put(value, "type", cJSON_CreateString(type));
    return value;
}

static void persistence_message(void)
{
    cJSON *value = message("persistence");
    json_put(value, "persisted", cJSON_CreateBool(!dirty));
    broadcast(value);
}

static bool persist(void)
{
    bool ok = store_save(state_path, &state);
    dirty = !ok;
    if (!ok) {
        fprintf(stderr, "agentd: cannot save state: %s; will retry\n", strerror(errno));
        retry_at = milliseconds(CLOCK_MONOTONIC) + 1000;
    }
    return ok;
}

static void changed(struct session *s)
{
    bool was_dirty = dirty;
    persist();
    cJSON *value = message("update");
    json_put(value, "session", session_json(s));
    json_put(value, "persisted", cJSON_CreateBool(!dirty));
    broadcast(value);
    if (dirty != was_dirty) persistence_message();
}

static void removed(struct session *s)
{
    char *id = copy_string(s->id);
    char *receipt = join(exit_dir, id);
    unlink(receipt); free(receipt);
    session_remove(&state, id);
    bool was_dirty = dirty;
    persist();
    cJSON *value = message("remove");
    json_put(value, "session", cJSON_CreateString(id));
    json_put(value, "persisted", cJSON_CreateBool(!dirty));
    broadcast(value);
    if (dirty != was_dirty) persistence_message();
    free(id);
}

static void snapshot(struct peer *p)
{
    cJSON *value = state_json(&state);
    json_put(value, "type", cJSON_CreateString("snapshot"));
    json_put(value, "persisted", cJSON_CreateBool(!dirty));
    json_put(value, "zmx_dir", cJSON_CreateString(zmx_dir));
    send_json(p, value);
}

static void reply(struct peer *p, const cJSON *request, const char *code, const char *error)
{
    cJSON *value = message("result");
    const cJSON *request_id = cJSON_GetObjectItemCaseSensitive(request, "request_id");
    if (request_id) json_put(value, "request_id", cJSON_Duplicate(request_id, true));
    json_put(value, "ok", cJSON_CreateBool(!error));
    json_put(value, "applied", cJSON_CreateBool(!error));
    json_put(value, "persisted", cJSON_CreateBool(!dirty));
    if (error) {
        json_put(value, "error", cJSON_CreateString(error));
        json_put(value, "error_code", cJSON_CreateString(code));
    }
    send_json(p, value);
}

static bool optional_text(const cJSON *request, const char *key)
{
    const cJSON *value = cJSON_GetObjectItemCaseSensitive(request, key);
    return !value || cJSON_IsNull(value) || (cJSON_IsString(value) && strlen(value->valuestring) <= 8192);
}

static void client_request(struct peer *p, const cJSON *request)
{
    const char *op = json_string(request, "op");
    const char *id = json_string(request, "session");
    const cJSON *request_id = cJSON_GetObjectItemCaseSensitive(request, "request_id");
    if (request_id && (!cJSON_IsString(request_id) || strlen(request_id->valuestring) > 128)) {
        reply(p, NULL, "invalid_request", "request_id must be a string of at most 128 bytes");
        return;
    }
    if (text_is(op, "list")) { snapshot(p); reply(p, request, NULL, NULL); return; }
    if (!valid_id(id)) { reply(p, request, "invalid_request", "session must be an ID of 1-128 letters, digits, hyphens or underscores"); return; }
    if (text_is(op, "register")) {
        const char *kind = json_string(request, "kind");
        const char *cwd = json_string(request, "cwd");
        if (!valid_kind(kind) || !cwd || cwd[0] != '/' || !optional_text(request, "cwd") || !optional_text(request, "title")) {
            reply(p, request, "invalid_request", "register requires kind, absolute cwd and optional string/null title"); return;
        }
        const cJSON *managed = cJSON_GetObjectItemCaseSensitive(request, "managed");
        if (managed && !cJSON_IsBool(managed)) { reply(p, request, "invalid_request", "managed must be boolean"); return; }
        if (cJSON_IsTrue(managed)) {
            char *path = join(zmx_dir, id);
            struct stat st;
            bool exists = lstat(path, &st) == 0 || errno != ENOENT;
            free(path);
            if (exists) { reply(p, request, "session_conflict", "zmx session path already exists or cannot be checked"); return; }
        }
        struct session *s = session_add(&state, id, kind, cwd, json_string(request, "title"), milliseconds(CLOCK_REALTIME));
        if (!s) { reply(p, request, "session_conflict", "session already registered or session limit reached"); return; }
        s->managed = cJSON_IsTrue(managed);
        s->launch_pending = s->exit_expected = s->managed;
        if (s->managed) s->launch_deadline = milliseconds(CLOCK_REALTIME) + 30000;
        s->lifecycle_revision = ++revision;
        changed(s);
        reply(p, request, NULL, NULL);
        return;
    }
    struct session *s = session_find(&state, id);
    if (!s) { reply(p, request, "unknown_session", "unknown session"); return; }
    if (text_is(op, "set_title")) {
        if (!cJSON_GetObjectItemCaseSensitive(request, "title") || !optional_text(request, "title")) {
            reply(p, request, "invalid_request", "title must be a string of at most 8192 bytes or null"); return;
        }
        replace_string(&s->title, json_string(request, "title"));
        changed(s);
        reply(p, request, NULL, NULL);
    } else if (text_is(op, "started")) {
        /* A late wrapper must never start a harness after its launch expired. */
        char *path = join(zmx_dir, id);
        struct stat st;
        bool socket_exists = lstat(path, &st) == 0 && S_ISSOCK(st.st_mode);
        free(path);
        if (!s->managed || !s->launch_pending || s->kill_requested || !socket_exists
            || milliseconds(CLOCK_REALTIME) >= s->launch_deadline) {
            reply(p, request, "launch_expired", "launch is no longer pending or zmx socket is absent"); return;
        }
        s->launch_pending = false;
        s->zmx_state = ZMX_ALIVE;
        s->lifecycle_revision = ++revision;
        changed(s); reply(p, request, NULL, NULL);
    } else if (text_is(op, "kill")) {
        if (!s->managed) { reply(p, request, "not_managed", "session is not managed by zmx"); return; }
        s->kill_requested = true;
        s->launch_pending = false;
        s->lifecycle_revision = ++revision;
        replace_string(&s->lifecycle_error, NULL);
        probe_at = 0;
        changed(s); reply(p, request, NULL, NULL); /* Accepted intent; removal confirms death. */
    } else if (text_is(op, "dismiss_error")) {
        if (!s->managed || !session_has_error(s) || s->zmx_state != ZMX_DEAD) {
            reply(p, request, "not_dismissible", "only a confirmed dead failed session can be dismissed"); return;
        }
        removed(s); reply(p, request, NULL, NULL);
    } else reply(p, request, "unknown_operation", "unknown operation");
}

static bool exit_receipt(struct session *s)
{
    if (!s->exit_expected || s->exit_code >= 0) return false;
    char *path = join(exit_dir, s->id);
    int fd = open(path, O_RDONLY | O_NONBLOCK | O_CLOEXEC | O_NOFOLLOW);
    free(path);
    if (fd < 0) return false;
    struct stat st;
    char buffer[1025];
    ssize_t n = -1;
    if (fstat(fd, &st) == 0 && S_ISREG(st.st_mode) && st.st_uid == geteuid() && st.st_size <= 1024)
        n = read(fd, buffer, 1024);
    close(fd);
    if (n < 0) return false;
    buffer[n] = 0;
    cJSON *value = json_parse(buffer, (size_t)n);
    const cJSON *code = cJSON_GetObjectItemCaseSensitive(value, "exit_code");
    bool ok = text_is(json_string(value, "session"), s->id) && cJSON_IsNumber(code)
        && code->valuedouble >= 0 && code->valuedouble <= 255 && code->valuedouble == code->valueint;
    if (ok) {
        s->exit_code = code->valueint;
        s->launch_pending = false;
        if (s->status != ERROR) {
            enum agent_status status = s->exit_code ? ERROR : EXITED;
            if (s->status != status) s->status_since = milliseconds(CLOCK_REALTIME);
            s->status = status;
            s->status_stale = false;
            if (s->exit_code) {
                char error[100];
                snprintf(error, sizeof(error), "Harness exited with status %d; inspect its terminal/logs", s->exit_code);
                replace_string(&s->message, error);
            }
        }
    }
    cJSON_Delete(value);
    return ok;
}

static void reconcile(const cJSON *rows)
{
    /* Failed/partial listings never provide negative evidence. */
    if (rows) {
        const cJSON *row;
        cJSON_ArrayForEach(row, rows) {
            if (session_find(&state, row->string) || !text_is(cJSON_GetStringValue(row), "alive")) continue;
            struct session *s = session_add(&state, row->string, "unknown", NULL, NULL, milliseconds(CLOCK_REALTIME));
            if (s) { s->managed = true; s->zmx_state = ZMX_ALIVE; s->lifecycle_revision = ++revision; changed(s); }
        }
    }
    for (struct session *s = state.sessions, *next; s; s = next) {
        next = s->next;
        if (!s->managed || s->lifecycle_revision > probe_revision) continue;
        bool update = exit_receipt(s);
        const char *seen = rows ? json_string(rows, s->id) : NULL;
        enum zmx_status observed = ZMX_UNKNOWN;
        const char *error = rows ? NULL : "zmx listing failed, timed out, or was incomplete; will retry";
        if (text_is(seen, "alive")) observed = ZMX_ALIVE;
        else if (text_is(seen, "dead")) observed = ZMX_DEAD;
        else if (text_is(seen, "unknown")) error = "zmx session did not respond; will retry";
        else if (rows) {
            char *path = join(zmx_dir, s->id);
            struct stat st;
            if (lstat(path, &st) < 0 && errno == ENOENT) observed = ZMX_DEAD;
            else error = "zmx socket exists but was not listed; will retry";
            free(path);
        }
        if (s->launch_pending) {
            if (milliseconds(CLOCK_REALTIME) < s->launch_deadline) observed = ZMX_UNKNOWN;
            else {
                s->launch_pending = false;
                s->status = ERROR;
                s->status_since = milliseconds(CLOCK_REALTIME);
                replace_string(&s->message, "Launch expired before the harness started");
                update = true;
            }
        }
        if (observed == ZMX_DEAD && !s->launch_pending) {
            /* Read terminal evidence after confirmed death, before deleting a
               normal exit. Large transcripts catch up over later passes. */
            update |= session_poll(s, milliseconds(CLOCK_REALTIME));
            if (!s->kill_requested && s->exit_expected && s->exit_code < 0 && s->status != ERROR) {
                s->status = ERROR;
                s->status_since = milliseconds(CLOCK_REALTIME);
                replace_string(&s->message, "Session disappeared without an exit result");
                update = true;
            }
            if (s->kill_requested || (!session_has_error(s) && !session_pending(s))) { removed(s); continue; }
        }
        if (s->zmx_state != observed) { s->zmx_state = observed; update = true; }
        if (!error && s->kill_requested) error = s->lifecycle_error;
        if ((error || s->lifecycle_error) && !text_is(error, s->lifecycle_error)) {
            replace_string(&s->lifecycle_error, error); update = true;
        }
        if (update) changed(s);
    }
}

static void lifecycle(void)
{
    int64_t now = milliseconds(CLOCK_MONOTONIC);
    if (zmx_job.pid && zmx_poll(&zmx_job, now)) {
        if (zmx_job.session) {
            struct session *s = session_find(&state, zmx_job.session);
            if (s && (zmx_job.failed || zmx_job.result)) {
                replace_string(&s->lifecycle_error, "zmx kill failed or timed out; awaiting confirmation and retrying");
                changed(s);
            }
            probe_next = true;
            probe_at = 0;
        } else {
            cJSON *rows = !zmx_job.failed && !zmx_job.result ? zmx_listing(zmx_job.output) : NULL;
            reconcile(rows);
            cJSON_Delete(rows);
            probe_next = false;
            probe_at = now + 2000;
        }
        zmx_dispose(&zmx_job);
    }
    if (zmx_job.pid || now < probe_at) return;
    const char *kill_id = NULL;
    if (!probe_next) {
        const char *first = NULL;
        for (struct session *s = state.sessions; s; s = s->next) {
            if (!s->managed || !s->kill_requested) continue;
            if (!first || strcmp(s->id, first) < 0) first = s->id;
            if ((!kill_cursor || strcmp(s->id, kill_cursor) > 0) && (!kill_id || strcmp(s->id, kill_id) < 0)) kill_id = s->id;
        }
        if (!kill_id) kill_id = first;
    }
    probe_revision = revision;
    if (!zmx_start(&zmx_job, zmx_program, zmx_dir, kill_id, now)) {
        reconcile(NULL);
        probe_at = now + 2000;
    } else if (kill_id) replace_string(&kill_cursor, kill_id);
}

static void line(struct peer *p, char *text)
{
    cJSON *value = json_parse(text, strlen(text));
    if (!cJSON_IsObject(value)) {
        if (!p->ingest) reply(p, NULL, "invalid_request", "expected one JSON object per line, without NULs");
        else fputs("agentd: rejected malformed hook\n", stderr);
    } else if (p->ingest) {
        struct session *s;
        int result = session_event(&state, value, milliseconds(CLOCK_REALTIME), &s);
        if (result > 0) changed(s);
        else if (result < 0) fputs("agentd: rejected invalid hook fields or harness kind\n", stderr);
        else fputs("agentd: ignored unassociated, unregistered, stale or unsupported hook\n", stderr);
    } else client_request(p, value);
    cJSON_Delete(value);
}

static void read_peer(struct peer *p)
{
    /* Bound work per poll iteration so a busy sender cannot monopolize it. */
    char buffer[16384];
    ssize_t count = read(p->fd, buffer, sizeof(buffer));
    if (count < 0) {
        if (errno != EAGAIN && errno != EWOULDBLOCK && errno != EINTR) drop(p);
        return;
    }
    if (!count) {
        p->eof = true;
        if (p->used) { fputs("agentd: discarded incomplete JSON line\n", stderr); drop(p); }
        else if (p->queued == p->sent) drop(p);
        return;
    }
    if (memchr(buffer, '\0', (size_t)count) || p->used + (size_t)count > MAX_LINE) {
        fputs("agentd: rejected NUL or oversized input\n", stderr);
        drop(p); return;
    }
    char *input = allocate(p->used + (size_t)count + 1);
    if (p->used) memcpy(input, p->input, p->used);
    memcpy(input + p->used, buffer, (size_t)count);
    free(p->input);
    p->input = input;
    if (!p->used) p->incomplete_since = milliseconds(CLOCK_MONOTONIC);
    p->used += (size_t)count;
    char *newline;
    while (p->fd >= 0 && (newline = memchr(p->input, '\n', p->used))) {
        size_t length = (size_t)(newline - p->input);
        char *record = allocate(length + 1);
        memcpy(record, p->input, length);
        p->used -= length + 1;
        memmove(p->input, newline + 1, p->used);
        p->input[p->used] = '\0';
        p->incomplete_since = (p->used || p->ingest) ? milliseconds(CLOCK_MONOTONIC) : 0;
        line(p, record);
        free(record);
    }
}

static void write_peer(struct peer *p)
{
    size_t amount = p->queued - p->sent;
    if (amount > 65536) amount = 65536;
    ssize_t count = write(p->fd, p->output + p->sent, amount);
    if (count < 0) {
        if (errno != EAGAIN && errno != EWOULDBLOCK && errno != EINTR) drop(p);
        return;
    }
    p->sent += (size_t)count;
    if (p->sent == p->queued) {
        free(p->output);
        p->output = NULL;
        p->sent = p->queued = 0;
        if (p->eof) drop(p);
    }
}

static int nonblocking(int fd)
{
    int flags = fcntl(fd, F_GETFL);
    return flags < 0 || fcntl(fd, F_SETFL, flags | O_NONBLOCK) < 0 || fcntl(fd, F_SETFD, FD_CLOEXEC) < 0 ? -1 : 0;
}

static void accept_peer(int listener, bool ingest)
{
    int fd = accept(listener, NULL, NULL);
    if (fd < 0) return;
    if (nonblocking(fd) < 0) { close(fd); return; }
    for (size_t i = 0; i < MAX_PEERS; i++) {
        if (peers[i].fd >= 0) continue;
        peers[i] = (struct peer){.fd = fd, .ingest = ingest,
                                .incomplete_since = ingest ? milliseconds(CLOCK_MONOTONIC) : 0};
        if (!ingest) snapshot(&peers[i]);
        return;
    }
    close(fd);
}

static int listener(const char *path)
{
    struct sockaddr_un address = {.sun_family = AF_UNIX};
    if (strlen(path) >= sizeof(address.sun_path)) { errno = ENAMETOOLONG; return -1; }
    strcpy(address.sun_path, path);
    struct stat st;
    if (lstat(path, &st) == 0) {
        if (!S_ISSOCK(st.st_mode)) { errno = EEXIST; return -1; }
        /* The runtime lock is held. Still refuse to unlink an active socket
           owned by a different program that does not use our lock. */
        int probe = socket(AF_UNIX, SOCK_STREAM | SOCK_NONBLOCK | SOCK_CLOEXEC, 0);
        if (probe < 0) return -1;
        int result = connect(probe, (struct sockaddr *)&address, sizeof(address));
        int error = errno;
        close(probe);
        if (!result || (error != ECONNREFUSED && error != ENOENT)) { errno = EADDRINUSE; return -1; }
        if (unlink(path) < 0 && errno != ENOENT) return -1;
    } else if (errno != ENOENT) return -1;
    int fd = socket(AF_UNIX, SOCK_STREAM, 0);
    if (fd < 0) return -1;
    if (nonblocking(fd) < 0 || bind(fd, (struct sockaddr *)&address, sizeof(address)) < 0) {
        int error = errno; close(fd); errno = error; return -1;
    }
    if (listen(fd, 64) < 0) {
        int error = errno; close(fd); unlink(path); errno = error; return -1;
    }
    return fd;
}

static bool directories(const char *path)
{
    char *copy = copy_string(path);
    for (char *p = copy + 1; ; p++) {
        if (*p != '/' && *p) continue;
        char saved = *p;
        *p = '\0';
        if (mkdir(copy, 0700) < 0 && errno != EEXIST) { free(copy); return false; }
        *p = saved;
        if (!saved) break;
    }
    free(copy);
    return true;
}

static char *join(const char *base, const char *suffix)
{
    char *result = allocate(strlen(base) + strlen(suffix) + 2);
    sprintf(result, "%s/%s", base, suffix);
    return result;
}

static int lock_path(const char *path)
{
    int fd = open(path, O_RDWR | O_CREAT | O_CLOEXEC | O_NOFOLLOW, 0600);
    if (fd < 0) return -1;
    if (flock(fd, LOCK_EX | LOCK_NB) < 0) { int error = errno; close(fd); errno = error; return -1; }
    return fd;
}

static bool serve(int ingest, int client)
{
    int64_t observe_at = 0;
    while (!stopping) {
        struct pollfd fds[MAX_PEERS + 2] = {{.fd = ingest, .events = POLLIN}, {.fd = client, .events = POLLIN}};
        for (size_t i = 0; i < MAX_PEERS; i++) {
            fds[i + 2].fd = peers[i].fd;
            fds[i + 2].events = (peers[i].eof ? 0 : POLLIN) | (peers[i].queued > peers[i].sent ? POLLOUT : 0);
        }
        int result = poll(fds, MAX_PEERS + 2, 100);
        if (result < 0 && errno != EINTR) { perror("poll"); return false; }
        for (size_t i = 0; i < MAX_PEERS; i++) {
            struct peer *p = &peers[i];
            if (p->fd < 0) continue;
            short ready = fds[i + 2].revents;
            if (ready & (POLLERR | POLLNVAL)) { drop(p); continue; }
            if (!p->eof && (ready & (POLLIN | POLLHUP))) read_peer(p);
            if (p->fd >= 0 && (ready & POLLOUT) && p->queued > p->sent) write_peer(p);
            if (p->fd >= 0 && p->incomplete_since && milliseconds(CLOCK_MONOTONIC) - p->incomplete_since >= INPUT_TIMEOUT_MS)
                drop(p);
        }
        /* Accept after processing the existing slots, so a newly accepted
           peer can't inherit a closed peer's readiness flags. */
        if (fds[0].revents & POLLIN) accept_peer(ingest, true);
        if (fds[1].revents & POLLIN) accept_peer(client, false);
        if (milliseconds(CLOCK_MONOTONIC) >= observe_at) {
            for (struct session *s = state.sessions; s; s = s->next)
                if (session_poll(s, milliseconds(CLOCK_REALTIME))) changed(s);
            observe_at = milliseconds(CLOCK_MONOTONIC) + 200;
        }
        if (dirty && milliseconds(CLOCK_MONOTONIC) >= retry_at) {
            if (persist()) persistence_message();
        }
        lifecycle();
    }
    return true;
}

int main(int argc, char **argv)
{
    const char *runtime = NULL;
    char *default_runtime = NULL, *default_state = NULL;
    for (int i = 1; i < argc; i++) {
        if (!strcmp(argv[i], "--runtime-dir") && i + 1 < argc) runtime = argv[++i];
        else if (!strcmp(argv[i], "--state-file") && i + 1 < argc) state_path = argv[++i];
        else if (!strcmp(argv[i], "--zmx-bin") && i + 1 < argc) zmx_program = argv[++i];
        else {
            fprintf(stderr, "usage: %s [--runtime-dir DIR] [--state-file FILE] [--zmx-bin PROGRAM]\n", argv[0]);
            return 2;
        }
    }
    if (!runtime) {
        const char *base = getenv("XDG_RUNTIME_DIR");
        if (!base || base[0] != '/') { fputs("agentd: set XDG_RUNTIME_DIR or --runtime-dir\n", stderr); return 2; }
        runtime = default_runtime = join(base, "agentd");
    }
    if (!state_path) {
        const char *base = getenv("XDG_STATE_HOME");
        if (base && base[0] == '/') default_state = join(base, "agentd/state.json");
        else {
            base = getenv("HOME");
            if (!base || base[0] != '/') { fputs("agentd: set XDG_STATE_HOME or --state-file\n", stderr); free(default_runtime); return 2; }
            default_state = join(base, ".local/state/agentd/state.json");
        }
        state_path = default_state;
    }
    int result = 1, runtime_lock = -1, state_lock = -1, ingest = -1, client = -1;
    char *ingest_path = join(runtime, "ingest.sock"), *client_path = join(runtime, "client.sock");
    char *runtime_lock_path = join(runtime, "server.lock");
    zmx_dir = join(runtime, "zmx"); exit_dir = join(runtime, "exits");
    char *state_lock_path = allocate(strlen(state_path) + 6);
    sprintf(state_lock_path, "%s.lock", state_path);
    char *parent = copy_string(state_path);
    if (runtime[0] != '/' || state_path[0] != '/' || !strrchr(parent, '/')[1]) {
        fputs("agentd: paths must be absolute and state-file must name a file\n", stderr); goto cleanup;
    }
    char *slash = strrchr(parent, '/');
    if (slash == parent) slash[1] = '\0'; else *slash = '\0';
    umask(0077);
    if (!directories(runtime) || !directories(parent)) { perror("agentd: create directories"); goto cleanup; }
    struct stat st;
    if (lstat(runtime, &st) < 0 || !S_ISDIR(st.st_mode) || st.st_uid != geteuid() || (st.st_mode & 0077)) {
        fputs("agentd: runtime directory must be owned by this user and private (0700)\n", stderr); goto cleanup;
    }
    if (!directories(zmx_dir) || !directories(exit_dir)) { perror("agentd: lifecycle directories"); goto cleanup; }
    runtime_lock = lock_path(runtime_lock_path);
    if (runtime_lock < 0) { perror("agentd: runtime lock"); goto cleanup; }
    state_lock = lock_path(state_lock_path);
    if (state_lock < 0) { perror("agentd: state lock"); goto cleanup; }
    if (!store_load(state_path, &state)) { perror("agentd: refusing invalid/unreadable state"); goto cleanup; }
    for (struct session *s = state.sessions; s; s = s->next) {
        s->status_stale = true;
        s->zmx_state = ZMX_UNKNOWN;
        for (struct session *c = s->children; c; c = c->next) c->status_stale = true;
    }
    struct sigaction action = {.sa_handler = stop};
    sigemptyset(&action.sa_mask);
    sigaction(SIGTERM, &action, NULL);
    sigaction(SIGINT, &action, NULL);
    signal(SIGPIPE, SIG_IGN);
    for (size_t i = 0; i < MAX_PEERS; i++) peers[i].fd = -1;
    ingest = listener(ingest_path);
    if (ingest < 0) { perror("agentd: ingest socket"); goto cleanup; }
    client = listener(client_path);
    if (client < 0) { perror("agentd: client socket"); goto cleanup; }
    if (state.count) persist();
    fprintf(stderr, "agentd: ready at %s\n", runtime);
    result = serve(ingest, client) ? 0 : 1;
    if (dirty) persist();
    for (size_t i = 0; i < MAX_PEERS; i++) drop(&peers[i]);
cleanup:
    zmx_dispose(&zmx_job);
    if (ingest >= 0) { close(ingest); unlink(ingest_path); }
    if (client >= 0) { close(client); unlink(client_path); }
    if (state_lock >= 0) close(state_lock);
    if (runtime_lock >= 0) close(runtime_lock);
    state_free(&state);
    free(parent); free(state_lock_path); free(runtime_lock_path);
    free(ingest_path); free(client_path); free(default_runtime); free(default_state);
    free(zmx_dir); free(exit_dir);
    free(kill_cursor);
    return result;
}
