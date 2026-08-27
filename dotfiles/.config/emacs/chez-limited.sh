#!/bin/sh
# Cap the Chez REPL's address space so a runaway eval (e.g. an accidentally
# non-terminating allocating loop) kills this one scheme process instead of
# pushing the emacs.service cgroup over memory.high and freezing the daemon
# (see 2026-08-24: two geiser REPLs at 15.7GB+6.6GB throttled Emacs into D
# state). Resolves `scheme` from PATH at exec time so each project's direnv
# still picks its own Chez. Override the cap with CHEZ_ULIMIT_KB.
ulimit -v "${CHEZ_ULIMIT_KB:-4194304}"   # 4 GiB
exec scheme "$@"
