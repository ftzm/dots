;;; agentd-terminal.el --- Ghostel attachments to persistent agents -*- lexical-binding: t; -*-
(require 'agentd-client)
(require 'agentd-status)
(require 'agentd-attention)
(require 'agentd-completion)

(declare-function ghostel-exec "ghostel" (buffer program &optional args identity))
(declare-function ghostel-mode "ghostel" ())
(defvar ghostel-environment)
(defvar ghostel-kill-buffer-on-exit)
(defvar ghostel-query-before-killing)
(defvar ghostel-exit-functions)
(defvar agentd-terminal-buffers (make-hash-table :test #'equal))
(defvar-local agentd-terminal-key nil)
(defvar-local agentd-terminal-process nil)
(defvar-local agentd-terminal-state nil)
(defvar-local agentd-terminal-restarting nil)
(defvar-local agentd-terminal-name nil "Name reserved by this agent buffer.")
(defvar-local agentd-terminal-pending-name nil)
(declare-function agentd-session-mode "agentd-session" (&optional arg))
(defvar agentd-terminal--preserve-session nil)
(defvar agentd-terminal-background nil)
(defvar agentd-terminal-open-hook nil)
(defvar agentd-terminal-detach-hook nil)
(defvar-local agentd-terminal--killing nil)
(defvar-local agentd-terminal--kill-timer nil)

(defun agentd-terminal--kill-finished (&optional failure)
  "Finish a kill request, closing only after confirmed session removal."
  (when agentd-terminal--kill-timer (cancel-timer agentd-terminal--kill-timer))
  (setq agentd-terminal--kill-timer nil agentd-terminal--killing nil)
  (remove-hook 'agentd-buffer-update-hook #'agentd-terminal--check-killed t)
  (if failure
      (message "Agent session: %s; buffer retained" failure)
    ;; Metadata hooks may still be iterating this buffer.  Close after they return.
    (let ((buffer (current-buffer)))
      (run-at-time 0 nil
                   (lambda ()
                     (when (buffer-live-p buffer)
                       (let ((agentd-terminal--preserve-session t)) (kill-buffer buffer))))))))

(defun agentd-terminal--check-killed ()
  (when (eq agentd-terminal--killing 'accepted)
    (cond
     ((and (eq agentd-connection-status 'connected) agentd-status--seen
           (not agentd-session))
      (agentd-terminal--kill-finished))
     ((and (eq (plist-get agentd-session :kill_requested) t)
           (stringp (plist-get agentd-session :lifecycle_error)))
      (agentd-terminal--kill-finished (plist-get agentd-session :lifecycle_error))))))

(defun agentd-terminal--kill-query ()
  "Ask agentd to kill this session; defer buffer deletion until confirmed.
Emacs exit does not run buffer kill queries.  Explicit detach bypasses this
query, and neither action sends a session kill request."
  (cond
   ((or agentd-terminal--preserve-session (not agentd-terminal-key)
        (and agentd-status--seen (not agentd-session)
             (eq agentd-connection-status 'connected))) t)
   (agentd-terminal--killing (message "Waiting for the agent session to end…") nil)
   (t
    (let ((buffer (current-buffer)) (key agentd-terminal-key)
          (agentd-runtime-directory (car agentd-terminal-key)))
      (setq agentd-terminal--killing 'requested)
      (agentd-status-watch)
      (add-hook 'agentd-buffer-update-hook #'agentd-terminal--check-killed nil t)
      (agentd-kill
       (cadr key)
       (lambda (_result failure)
         (when (and (buffer-live-p buffer)
                    (equal key (buffer-local-value 'agentd-terminal-key buffer)))
           (with-current-buffer buffer
             (if failure
                 ;; A failed launch may never have registered a session.  The
                 ;; server's explicit absence reply also confirms we can close.
                 (agentd-terminal--kill-finished
                  (unless (and (eq (plist-get failure :kind) 'server)
                               (equal (plist-get failure :code) "unknown_session"))
                    (plist-get failure :message)))
               (setq agentd-terminal--killing 'accepted
                     agentd-terminal--kill-timer
                     (run-at-time
                      10 nil
                      (lambda ()
                        (when (buffer-live-p buffer)
                          (with-current-buffer buffer
                            (agentd-terminal--kill-finished "Session termination not yet confirmed"))))))
               (agentd-terminal--check-killed)))))))
    nil)))

(defun agentd-terminal-name-used-p (name &optional except)
  "Whether NAME belongs to another buffer, ignoring EXCEPT."
  (or (let ((buffer (get-buffer (format "*agent:%s*" name))))
        (and buffer (not (eq buffer except))))
      (cl-some (lambda (buffer)
                 (and (not (eq buffer except))
                      (buffer-local-value 'agentd-terminal-key buffer)
                      (or (equal name (buffer-local-value 'agentd-terminal-name buffer))
                          (equal name (buffer-local-value 'agentd-terminal-pending-name buffer))
                          (equal name (buffer-local-value 'agentd-title buffer)))))
               (buffer-list))))

(defun agentd-terminal-check-name (name &optional except)
  "Validate NAME, ignoring EXCEPT when checking existing buffer names."
  (unless (and (stringp name) (not (string-empty-p (string-trim name)))
               (<= (string-bytes name) 8192) (not (string-match-p "[[:cntrl:]]" name)))
    (user-error "Enter a nonempty session name without control characters"))
  (when (agentd-terminal-name-used-p name except)
    (user-error "An agent buffer already uses the name %s" name)))

(defun agentd-terminal--sync-name ()
  "Reflect persistent titles in buffer names, disambiguating external titles."
  (when (and agentd-terminal-key agentd-title)
    (let ((name agentd-title))
      (when (agentd-terminal-name-used-p name (current-buffer))
        (setq name (format "%s [%s]" name (cadr agentd-terminal-key))))
      (setq agentd-terminal-name name)
      (unless (equal (buffer-name) (format "*agent:%s*" name))
        (rename-buffer (format "*agent:%s*" name) t)))))

(defun agentd-terminal--cleanup ()
  (when agentd-terminal--kill-timer (cancel-timer agentd-terminal--kill-timer))
  (when (eq (gethash agentd-terminal-key agentd-terminal-buffers) (current-buffer))
    (remhash agentd-terminal-key agentd-terminal-buffers)))

(defun agentd-terminal--open (description)
  "Run DESCRIPTION in a Ghostel buffer; reuse an existing live attachment."
  (require 'ghostel)
  (require 'agentd-session)
  (let* ((id (plist-get description :id))
         (runtime (plist-get description :runtime))
         (key (list runtime id))
         (buffer (gethash key agentd-terminal-buffers)))
    (unless (buffer-live-p buffer)
      (setq buffer (generate-new-buffer (format "*agent:%s*" id)))
      (with-current-buffer buffer (ghostel-mode)))
    (unless agentd-terminal-background (switch-to-buffer buffer))
    (with-current-buffer buffer
      (unless (process-live-p agentd-terminal-process)
        (setq default-directory (file-name-as-directory (plist-get description :directory)))
        (setq-local ghostel-kill-buffer-on-exit nil ghostel-query-before-killing nil)
        (let ((ghostel-environment
               (cons (concat "AGENTD_RUNTIME_DIR=" runtime)
                     (cl-remove-if (lambda (s) (string-match-p "\\`AGENTD_RUNTIME_DIR\\(?:=\\|\\'\\)" s))
                                   ghostel-environment))))
          (setq agentd-terminal-process
                (ghostel-exec buffer (plist-get description :program) (plist-get description :args)
                              `((kind . agentd) (runtime . ,runtime) (session . ,id)))))
        (setq agentd-terminal-key key agentd-terminal-state 'attached)
        (puthash key buffer agentd-terminal-buffers)
        ;; Capture the attachment generation: stale sentinels cannot update a
        ;; replacement process in this same buffer. Preserve Ghostel's sentinel.
        (let* ((process agentd-terminal-process) (sentinel (process-sentinel process)))
          (set-process-sentinel
           process (lambda (p event)
                     (when (and (buffer-live-p buffer)
                                (eq p (buffer-local-value 'agentd-terminal-process buffer)))
                       (when sentinel (funcall sentinel p event))
                       (when (and (buffer-live-p buffer) (not (process-live-p p)))
                         (with-current-buffer buffer (setq agentd-terminal-state 'detached)))))))
        (add-hook 'kill-buffer-hook #'agentd-terminal--cleanup nil t)
        (add-hook 'kill-buffer-query-functions #'agentd-terminal--kill-query nil t)
        (add-hook 'agentd-buffer-update-hook #'agentd-terminal--sync-name nil t)
        (add-hook 'change-major-mode-hook #'agentd-terminal--cleanup nil t))
      (when-let* ((title (plist-get description :title)))
        (setq agentd-terminal-name title)
        (rename-buffer (format "*agent:%s*" title)))
      (agentd-session-mode 1)
      (agentd-status-watch)
      (run-hooks 'agentd-terminal-open-hook))
    buffer))

(defun agentd-terminal-new (kind directory &optional arguments title)
  "Launch KIND in DIRECTORY with argv ARGUMENTS in Ghostel."
  (when (and title (agentd-terminal-name-used-p title))
    (user-error "An agent buffer already uses the name %s" title))
  (agentd-terminal--open (agentd-launch-description kind directory arguments title)))

(defun agentd-terminal-attach (id &optional directory)
  "Attach to ID in Ghostel. DIRECTORY supplies the local buffer directory."
  (agentd-terminal--open (agentd-attach-description id directory)))

(defun agentd-terminal--detach-process ()
  "Disconnect the terminal process without ending its persistent session."
  (unless agentd-terminal-key (user-error "Not an agent terminal"))
  ;; Ghostel's sentinel terminates the native child when its event pipe is
  ;; deleted; the Emacs PTY backend closes its attachment in the usual way.
  (when (process-live-p agentd-terminal-process) (delete-process agentd-terminal-process))
  (setq agentd-terminal-state 'detached))

(defun agentd-terminal-detach (&optional keep-buffer)
  "Detach and close this buffer, leaving the session running.
With a prefix argument KEEP-BUFFER, retain the disconnected buffer."
  (interactive "P")
  (when agentd-terminal--killing (user-error "Session termination is already in progress"))
  (agentd-terminal--detach-process)
  (unless keep-buffer
    (run-hooks 'agentd-terminal-detach-hook)
    (let ((agentd-terminal--preserve-session t)) (kill-buffer (current-buffer)))))

(defun agentd-terminal-reconnect ()
  "Reconnect this buffer to the same session."
  (interactive)
  (unless agentd-terminal-key (user-error "Not an agent terminal"))
  (let ((agentd-runtime-directory (car agentd-terminal-key)))
    (agentd-terminal-attach (cadr agentd-terminal-key) default-directory)))

;;;###autoload
(defun agentd-terminal-restart (&optional callback)
  "Explicitly restart idle Claude or Codex and reconstruct this buffer's history.
Keep the agent ID, title, launch options and environment.  Discard any unsent
input.  Never runs automatically on resize.  CALLBACK, when non-nil,
receives (RESULT ERROR), as with `agentd-request'."
  (interactive)
  (unless agentd-terminal-key (user-error "Not an agent terminal"))
  (when agentd-terminal-restarting (user-error "A restart is already in progress"))
  (let ((buffer (current-buffer))
        (key agentd-terminal-key)
        (process agentd-terminal-process)
        (agentd-runtime-directory (car agentd-terminal-key)))
    (setq agentd-terminal-restarting t)
    (message "Restarting agent and resuming its conversation…")
    (agentd-restart
     (cadr key)
     (lambda (result failure)
       (when (and (buffer-live-p buffer)
                  (equal key (buffer-local-value 'agentd-terminal-key buffer)))
         (with-current-buffer buffer
           (setq agentd-terminal-restarting nil)
           (when (and (not failure) (eq process agentd-terminal-process))
             (condition-case problem
                 (progn (agentd-terminal--detach-process) (agentd-terminal-reconnect))
               (error (setq failure (list :kind 'attachment :message (error-message-string problem))))))))
       (if failure
           (message "Agent restart: %s" (plist-get failure :message))
         (message "Agent resumed; terminal history reconstructed"))
       (when callback (funcall callback result failure))))))

(provide 'agentd-terminal)
;;; agentd-terminal.el ends here
