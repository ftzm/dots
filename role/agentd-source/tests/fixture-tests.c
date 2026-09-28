#include "session.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(void)
{
    FILE *manifest = fopen("tests/fixtures/hooks/cases.json", "r");
    assert(manifest);
    char contents[16384];
    size_t length = fread(contents, 1, sizeof(contents) - 1, manifest);
    assert(!ferror(manifest) && feof(manifest));
    fclose(manifest);
    contents[length] = '\0';
    cJSON *cases = json_parse(contents, length);
    assert(cJSON_IsArray(cases));
    struct state state = {0};
    int64_t now = 1000;
    const cJSON *test;
    cJSON_ArrayForEach(test, cases) {
        const char *name = json_string(test, "file");
        char path[256];
        assert(name && snprintf(path, sizeof(path), "tests/fixtures/hooks/%s.jsonl", name) < (int)sizeof(path));
        FILE *input = fopen(path, "r");
        assert(input);
        char *line = NULL;
        size_t capacity = 0;
        ssize_t n;
        int index = 0;
        struct session *session = NULL;
        while ((n = getline(&line, &capacity, input)) >= 0) {
            cJSON *event = json_parse(line, (size_t)n);
            assert(event);
            const char *id = json_string(event, "agent_session");
            session = session_find(&state, id);
            if (!session) session = session_add(&state, id, json_string(event, "agent_kind"), "/verification/work", "Keep my title", now);
            assert(session);
            struct session *changed = NULL;
            assert(session_event(&state, event, now++, &changed) >= 0);
            cJSON *record = session_json(session);
            const char *expected = cJSON_GetStringValue(cJSON_GetArrayItem(cJSON_GetObjectItemCaseSensitive(test, "statuses"), index));
            const char *actual = json_string(record, "status");
            if (!text_is(expected, actual)) fprintf(stderr, "%s event %d (%s): expected %s, got %s\n", name, index, json_string(event, "hook_event_name"), expected, actual);
            assert(text_is(expected, actual));
            assert(text_is(session->title, "Keep my title"));
            cJSON_Delete(record);
            cJSON_Delete(event);
            index++;
        }
        assert(!ferror(input));
        assert(index == cJSON_GetArraySize(cJSON_GetObjectItemCaseSensitive(test, "statuses")));
        const char *message = json_string(test, "message");
        if (message) assert(session && session->message && strstr(session->message, message));
        free(line);
        fclose(input);
    }
    printf("PASS: %d captured harness sequences replayed through state adapters\n", cJSON_GetArraySize(cases));
    state_free(&state);
    cJSON_Delete(cases);
    return 0;
}
