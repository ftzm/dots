# Working with Emacs

- After changing Emacs Lisp in this directory, load the changed definitions into the running Emacs daemon so the user can verify them manually before the task is considered done.
- Use `emacsclient --eval` to update only the changed definitions when possible. Avoid reloading all of `init.el` during an active session because startup forms can have side effects.
- Verify the loaded definitions in the daemon and report any connection or evaluation failure. Do not claim that a file edit is active in Emacs until the daemon confirms it.
- ALWAYS RUN THE CHANGED CODE before saying the task is done. Exercise the actual command or UI path in the running Emacs daemon; checking that a function exists or that a file parses is insufficient. If execution cannot be verified, say so explicitly.
- Preserve the user's open buffers and session; do not restart the daemon just to apply an Emacs Lisp change.
