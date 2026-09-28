#include "session.h"

/* Native lifecycle hooks, not the legacy argv-based notify interface. */
bool codex_event(const cJSON *event, struct transition *t)
{
    const char *name = json_string(event, "hook_event_name");
    t->informative = true;
    if (text_is(name, "SessionStart")) t->status = IDLE;
    else if (text_is(name, "UserPromptSubmit")) { t->status = WORKING; t->recovery = true; }
    else if (text_is(name, "PreToolUse")) {
        t->status = text_is(json_string(event, "tool_name"), "request_user_input") ? WAITING : WORKING;
        if (t->status == WAITING) t->message = "Waiting for an answer";
    } else if (text_is(name, "PostToolUse")) t->status = WORKING;
    else if (text_is(name, "PermissionRequest")) {
        t->status = WAITING;
        t->message = "Permission requested";
        t->notification_type = "permission_prompt";
    } else if (text_is(name, "Stop")) t->status = IDLE;
    else if (text_is(name, "SessionEnd")) t->status = EXITED;
    else if (text_is(name, "Interrupt")) {
        t->status = UNKNOWN;
        t->stale = true; /* Interruption doesn't prove readiness for another prompt. */
    } else if (text_is(name, "PreCompact") || text_is(name, "PostCompact")) {
        t->informative = false;
    } else return false; /* In particular, Codex has no verified StopFailure. */
    return true;
}
