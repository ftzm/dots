#include "session.h"

/* Captured hook mappings. transcript.c supplies missing terminal evidence. */
bool claude_event(const cJSON *event, struct transition *t)
{
    const char *name = json_string(event, "hook_event_name");
    t->informative = true;
    if (text_is(name, "SessionStart")) t->status = IDLE;
    else if (text_is(name, "UserPromptSubmit")) { t->status = WORKING; t->recovery = true; }
    else if (text_is(name, "PreToolUse")) {
        t->status = text_is(json_string(event, "tool_name"), "AskUserQuestion") ? WAITING : WORKING;
        if (t->status == WAITING) t->message = "Waiting for an answer";
    } else if (text_is(name, "PostToolUse") || text_is(name, "PostToolUseFailure")) t->status = WORKING;
    else if (text_is(name, "PermissionRequest")) {
        t->status = WAITING;
        t->message = "Permission requested";
        t->notification_type = "permission_prompt";
    } else if (text_is(name, "Elicitation")) {
        t->status = WAITING;
        t->message = json_string(event, "message");
        if (!t->message) t->message = "Waiting for input";
    } else if (text_is(name, "ElicitationResult")) t->status = WORKING;
    else if (text_is(name, "StopFailure")) {
        t->status = ERROR;
        t->message = json_string(event, "error_details");
        if (!t->message) t->message = json_string(event, "last_assistant_message");
        if (!t->message) t->message = json_string(event, "error");
        if (!t->message) t->message = "Agent turn failed";
    } else if (text_is(name, "Stop")) {
        t->status = IDLE;
        /* Subagent activity is tracked separately by session.c. Other task
           kinds/crons still lack a verified completion signal. */
        const cJSON *tasks = cJSON_GetObjectItemCaseSensitive(event, "background_tasks");
        const cJSON *crons = cJSON_GetObjectItemCaseSensitive(event, "session_crons");
        if ((tasks && !cJSON_IsNull(tasks) && !cJSON_IsArray(tasks))
            || (crons && !cJSON_IsNull(crons) && !cJSON_IsArray(crons))) {
            t->status = UNKNOWN; t->stale = true;
        }
        const cJSON *task;
        cJSON_ArrayForEach(task, tasks) {
            if (!text_is(json_string(task, "type"), "subagent") && text_is(json_string(task, "status"), "running")) {
                t->status = UNKNOWN; t->stale = true;
            }
        }
        if (cJSON_IsArray(crons) && cJSON_GetArraySize(crons)) { t->status = UNKNOWN; t->stale = true; }
    } else if (text_is(name, "SessionEnd")) {
        const char *reason = json_string(event, "reason");
        bool reset = text_is(reason, "clear") || text_is(reason, "resume");
        t->status = reset ? UNKNOWN : EXITED;
        t->stale = reset;
    } else if (text_is(name, "Notification")) {
        /* Notifications can arrive late; PermissionRequest is the waiting
           signal. Preserve status instead of overwriting a newer event. */
        t->informative = false;
    } else if (text_is(name, "PreCompact") || text_is(name, "PostCompact")) {
        t->informative = false;
    } else return false;
    return true;
}
