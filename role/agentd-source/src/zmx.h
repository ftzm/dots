#ifndef AGENTD_ZMX_H
#define AGENTD_ZMX_H
#include "session.h"
#include <sys/types.h>

struct zmx_job {
    pid_t pid;
    int fd, result;
    bool eof, reaped, failed;
    int64_t deadline;
    char *output, *session;
    size_t used;
};
bool zmx_start(struct zmx_job *job, const char *program, const char *directory,
               const char *kill_session, int64_t now);
bool zmx_poll(struct zmx_job *job, int64_t now);
void zmx_dispose(struct zmx_job *job);
/* Full successful listings only. Any malformed/partial row invalidates all. */
cJSON *zmx_listing(const char *output);
#endif
