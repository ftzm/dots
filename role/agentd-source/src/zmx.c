#include "zmx.h"
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

bool zmx_start(struct zmx_job *j, const char *program, const char *directory,
               const char *kill_session, int64_t now)
{
    int pipefd[2];
    if (pipe(pipefd) < 0) return false;
    fcntl(pipefd[0], F_SETFL, O_NONBLOCK);
    fcntl(pipefd[0], F_SETFD, FD_CLOEXEC);
    fcntl(pipefd[1], F_SETFD, FD_CLOEXEC);
    pid_t pid = fork();
    if (pid < 0) { close(pipefd[0]); close(pipefd[1]); return false; }
    if (!pid) {
        setpgid(0, 0);
        dup2(pipefd[1], STDOUT_FILENO);
        int null = open("/dev/null", O_RDWR);
        if (null < 0) _exit(127);
        dup2(null, STDIN_FILENO); dup2(null, STDERR_FILENO);
        if (null > 2) close(null);
        close(pipefd[0]); close(pipefd[1]);
        setenv("ZMX_DIR", directory, 1);
        setenv("ZMX_DIR_MODE", "0700", 1);
        setenv("ZMX_LOG_MODE", "0600", 1);
        unsetenv("ZMX_SESSION"); unsetenv("ZMX_SESSION_PREFIX");
        if (kill_session) execlp(program, program, "kill", kill_session, (char *)NULL);
        else execlp(program, program, "list", (char *)NULL);
        _exit(127);
    }
    close(pipefd[1]);
    setpgid(pid, pid);
    *j = (struct zmx_job){.pid = pid, .fd = pipefd[0], .deadline = now + 3000,
                         .session = copy_string(kill_session), .output = copy_string("")};
    return true;
}

bool zmx_poll(struct zmx_job *j, int64_t now)
{
    char buffer[65536];
    if (!j->pid) return false;
    if (!j->eof) {
        ssize_t n = read(j->fd, buffer, sizeof(buffer));
        if (!n) j->eof = true;
        else if (n < 0 && errno != EINTR && errno != EAGAIN && errno != EWOULDBLOCK) {
            j->failed = true; j->eof = true;
        } else if (n > 0) {
            if (j->used + (size_t)n > 1024 * 1024 || memchr(buffer, 0, (size_t)n)) j->failed = true;
            else {
                char *next = allocate(j->used + (size_t)n + 1);
                memcpy(next, j->output, j->used);
                memcpy(next + j->used, buffer, (size_t)n);
                free(j->output); j->output = next; j->used += (size_t)n;
            }
        }
    }
    if (now >= j->deadline || j->failed) {
        if (!j->reaped) kill(-j->pid, SIGKILL);
        j->failed = true;
    }
    if (!j->reaped) {
        int status;
        pid_t result = waitpid(j->pid, &status, WNOHANG);
        if (result == j->pid) {
            j->reaped = true;
            j->result = WIFEXITED(status) ? WEXITSTATUS(status) : 128;
        }
    }
    return j->reaped && (j->eof || j->failed);
}

void zmx_dispose(struct zmx_job *j)
{
    if (!j->pid) return;
    if (!j->reaped) {
        kill(-j->pid, SIGKILL);
        while (waitpid(j->pid, NULL, 0) < 0 && errno == EINTR) {}
    }
    close(j->fd); free(j->output); free(j->session);
    *j = (struct zmx_job){0};
}

cJSON *zmx_listing(const char *output)
{
    cJSON *rows = cJSON_CreateObject();
    char *copy = copy_string(output), *line = copy;
    while (*line) {
        char *end = strchr(line, '\n');
        if (!end) goto invalid;
        *end = 0;
        while (*line == ' ') line++;
        if (strncmp(line, "name=", 5)) goto invalid;
        char *tab = strchr(line, '\t');
        if (!tab) goto invalid;
        *tab++ = 0;
        const char *id = line + 5, *status = NULL;
        if (!valid_id(id) || cJSON_GetObjectItemCaseSensitive(rows, id) || cJSON_GetArraySize(rows) >= 256) goto invalid;
        if (!strncmp(tab, "pid=", 4)) {
            char *after;
            long pid = strtol(tab + 4, &after, 10);
            if (pid <= 0 || after == tab + 4 || *after != '\t' || strncmp(after, "\tclients=", 9)) goto invalid;
            status = "alive";
        } else if (!strncmp(tab, "err=", 4)) {
            char *after = strchr(tab, '\t');
            if (!after || strncmp(after, "\tstatus=", 8)) goto invalid;
            *after = 0;
            if (!tab[4]) goto invalid;
            status = text_is(tab + 4, "ConnectionRefused") ? "dead" : "unknown";
        } else goto invalid;
        json_put(rows, id, cJSON_CreateString(status));
        line = end + 1;
    }
    free(copy);
    return rows;
invalid:
    free(copy); cJSON_Delete(rows); return NULL;
}
