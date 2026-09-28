#ifndef AGENTD_STORE_H
#define AGENTD_STORE_H
#include "session.h"

/* Missing file is an empty state; invalid/unreadable files are fatal to startup. */
bool store_load(const char *path, struct state *state);
/* Atomic replacement, without fsync/power-loss durability. errno on failure. */
bool store_save(const char *path, const struct state *state);
#endif
