#include "store.h"

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

#define MAX_STATE_BYTES (16 * 1024 * 1024)

bool store_load(const char *path, struct state *state)
{
    int fd = open(path, O_RDONLY | O_CLOEXEC | O_NONBLOCK);
    if (fd < 0) return errno == ENOENT;
    struct stat st;
    if (fstat(fd, &st) < 0) { close(fd); return false; }
    if (!S_ISREG(st.st_mode) || st.st_size < 1 || st.st_size > MAX_STATE_BYTES) {
        close(fd); errno = EINVAL; return false;
    }
    size_t length = (size_t)st.st_size;
    char *data = allocate(length + 1);
    size_t used = 0;
    while (used < length) {
        ssize_t got = read(fd, data + used, length - used);
        if (got < 0 && errno == EINTR) continue;
        if (got <= 0) { free(data); close(fd); errno = EIO; return false; }
        used += (size_t)got;
    }
    close(fd);
    cJSON *root = json_parse(data, length);
    bool valid = root && state_decode(state, root);
    cJSON_Delete(root);
    free(data);
    if (!valid) errno = EINVAL;
    return valid;
}

bool store_save(const char *path, const struct state *state)
{
    cJSON *root = state_json(state);
    char *data = json_print(root);
    cJSON_Delete(root);
    size_t length = strlen(data);
    if (length > MAX_STATE_BYTES) { free(data); errno = EFBIG; return false; }
    size_t path_length = strlen(path);
    char *temp = allocate(path_length + sizeof(".tmp.XXXXXX"));
    memcpy(temp, path, path_length);
    memcpy(temp + path_length, ".tmp.XXXXXX", sizeof(".tmp.XXXXXX"));
    int fd = mkstemp(temp);
    if (fd < 0) { free(temp); free(data); return false; }
    bool ok = true;
    size_t written = 0;
    while (written < length) {
        ssize_t count = write(fd, data + written, length - written);
        if (count < 0 && errno == EINTR) continue;
        if (count <= 0) { if (!count) errno = EIO; ok = false; break; }
        written += (size_t)count;
    }
    int saved_errno = errno;
    if (close(fd) < 0 && ok) { ok = false; saved_errno = errno; }
    if (ok && rename(temp, path) < 0) { ok = false; saved_errno = errno; }
    if (!ok) unlink(temp);
    free(temp);
    free(data);
    if (!ok) errno = saved_errno;
    return ok;
}
