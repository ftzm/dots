/* A foreground hook transport/parser probe, not the state daemon. */
#include "cJSON.h"

#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/un.h>
#include <unistd.h>

#define MAX_EVENT_BYTES (1024 * 1024)

static volatile sig_atomic_t stopping;

static void stop(int signal_number)
{
    (void)signal_number;
    stopping = 1;
}

static int print_event(const char *input, size_t length)
{
    cJSON *event = cJSON_ParseWithLengthOpts(input, length + 1, NULL, 1);
    const cJSON *type = cJSON_GetObjectItemCaseSensitive(event, "hook_event_name");
    if (!cJSON_IsObject(event) || !cJSON_IsString(type)) {
        fprintf(stderr, "rejected event: expected JSON object with string hook_event_name\n");
        cJSON_Delete(event);
        return 0;
    }

    /* Extract into a new object to demonstrate parsing, not byte forwarding. */
    const char *fields[] = {
        "agent_session", "agent_kind", "hook_event_name", "session_id", "cwd",
        "transcript_path", "source", "notification_type", "message", "received_at"
    };
    cJSON *summary = cJSON_CreateObject();
    if (!summary) {
        cJSON_Delete(event);
        return -1;
    }
    for (size_t i = 0; i < sizeof(fields) / sizeof(fields[0]); i++) {
        const cJSON *value = cJSON_GetObjectItemCaseSensitive(event, fields[i]);
        if (!value)
            continue;
        if (!cJSON_IsString(value) && !cJSON_IsNull(value)) {
            fprintf(stderr, "rejected event: %s must be a string or null\n", fields[i]);
            cJSON_Delete(summary);
            cJSON_Delete(event);
            return 0;
        }
        cJSON *copy = cJSON_Duplicate(value, 0);
        if (!copy || !cJSON_AddItemToObject(summary, fields[i], copy)) {
            cJSON_Delete(copy);
            cJSON_Delete(summary);
            cJSON_Delete(event);
            return -1;
        }
    }
    char *output = cJSON_PrintUnformatted(summary);
    int result = output && puts(output) >= 0 && fflush(stdout) == 0 ? 0 : -1;
    cJSON_free(output);
    cJSON_Delete(summary);
    cJSON_Delete(event);
    return result;
}

static int receive_event(int client, char *input)
{
    size_t used = 0;
    const struct timeval timeout = { .tv_sec = 1 };
    if (setsockopt(client, SOL_SOCKET, SO_RCVTIMEO, &timeout, sizeof(timeout)) < 0)
        return -1;
    while (!stopping) {
        ssize_t count = read(client, input + used, MAX_EVENT_BYTES - used);
        if (count < 0) {
            if (errno == EINTR)
                continue;
            fprintf(stderr, "discarded incomplete event: %s\n", strerror(errno));
            return 0;
        }
        if (count == 0)
            break;
        if (memchr(input + used, '\0', (size_t)count)) {
            fprintf(stderr, "rejected event: literal NUL byte\n");
            return 0;
        }
        used += (size_t)count;
        /* The forwarder sends one compact JSON line per connection. */
        if (memchr(input, '\n', used))
            break;
        if (used == MAX_EVENT_BYTES) {
            fprintf(stderr, "rejected event: exceeds size limit\n");
            return 0;
        }
    }
    if (!used || stopping)
        return 0;
    input[used] = '\0';
    return print_event(input, used);
}

int main(int argc, char **argv)
{
    struct sockaddr_un address = { .sun_family = AF_UNIX };
    if (argc != 2 || strlen(argv[1]) >= sizeof(address.sun_path)) {
        fprintf(stderr, "usage: %s SOCKET_PATH (parent directory must exist)\n", argv[0]);
        return 2;
    }
    strcpy(address.sun_path, argv[1]);
    umask(0077);
    struct sigaction action = { .sa_handler = stop };
    sigemptyset(&action.sa_mask);
    sigaction(SIGINT, &action, NULL);
    sigaction(SIGTERM, &action, NULL);
    signal(SIGPIPE, SIG_IGN);

    int listener = socket(AF_UNIX, SOCK_STREAM, 0);
    if (listener < 0) {
        perror("socket");
        return 1;
    }
    /* Do not unlink an existing socket: another server may own it. */
    if (bind(listener, (struct sockaddr *)&address, sizeof(address)) < 0) {
        perror("bind");
        close(listener);
        return 1;
    }
    int result = 1;
    char *input = malloc(MAX_EVENT_BYTES + 1);
    if (!input || listen(listener, 16) < 0) {
        fprintf(stderr, "could not initialize listener\n");
        goto cleanup;
    }
    fprintf(stderr, "listening on %s (cJSON %s)\n", argv[1], cJSON_Version());
    result = 0;
    while (!stopping) {
        int client = accept(listener, NULL, NULL);
        if (client < 0) {
            if (errno == EINTR)
                continue;
            perror("accept");
            result = 1;
            break;
        }
        int received = receive_event(client, input);
        close(client);
        if (received < 0) {
            fprintf(stderr, "could not process event\n");
            result = 1;
            break;
        }
    }
cleanup:
    free(input);
    close(listener);
    unlink(argv[1]);
    return result;
}
