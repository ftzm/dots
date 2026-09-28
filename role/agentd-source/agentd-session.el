;;; agentd-session.el --- Current agent controls -*- lexical-binding: t; -*-
(require 'agentd-terminal)
(require 'transient)
(require 'agentd-overview)
(declare-function agentd-new "agentd" (&optional kind directory arguments title))
(declare-function agentd-attach "agentd" ())
(declare-function agentd-attach-all "agentd" ())

(defun agentd-session--buffer-p () (and agentd-terminal-key t))

(defun agentd-session--check ()
  (unless agentd-terminal-key (user-error "Not an agent buffer")))

(defun agentd-session--status ()
  (format "%s%s%s" agentd-status
          (concat (if agentd-status-stale-p " (stale)" "")
                  (unless agentd-persisted-p (propertize " UNSAVED" 'face 'warning)))
          (if agentd-terminal--killing " — stopping" "")))

(defun agentd-session--summary ()
  (format "%s · %s · %s"
          (or agentd-terminal-name (cadr agentd-terminal-key) "Agent")
          agentd-kind (agentd-session--status)))

;;;###autoload
(defun agentd-rename (name)
  "Rename this session to unique NAME, persisting the title in agentd."
  (interactive (progn (agentd-session--check)
                      (list (read-string "Agent name: " (or agentd-title agentd-terminal-name)))))
  (agentd-session--check)
  (when agentd-terminal-pending-name (user-error "A rename is already in progress"))
  (agentd-terminal-check-name name (current-buffer))
  (let ((buffer (current-buffer)) (key agentd-terminal-key)
        (agentd-runtime-directory (car agentd-terminal-key)))
    (setq agentd-terminal-pending-name name)
    (agentd-set-title
     (cadr key) name
     (lambda (result failure)
       (when (and (buffer-live-p buffer)
                  (equal key (buffer-local-value 'agentd-terminal-key buffer)))
         (with-current-buffer buffer
           (setq agentd-terminal-pending-name nil)
           (unless failure
             (setq agentd-title name)
             (agentd-terminal--sync-name))))
       (message "%s" (if failure (concat "Rename failed: " (plist-get failure :message))
                       (format "Agent renamed to %s%s" name
                               (if (eq (plist-get result :persisted) :false)
                                   " (not yet persisted)" ""))))))))

;;;###autoload
(defun agentd-stop ()
  "End this agent session and close its buffer after confirmed termination."
  (interactive)
  (agentd-session--check)
  (kill-buffer (current-buffer)))

(defun agentd-session--dismissible-p ()
  (and (not agentd-status-stale-p) (eq agentd-status 'error)
       (equal (plist-get agentd-session :zmx_state) "dead")))

;;;###autoload
(defun agentd-dismiss-failure ()
  "Dismiss this confirmed dead failed session; retain its terminal output."
  (interactive)
  (agentd-session--check)
  (unless (agentd-session--dismissible-p)
    (user-error "Only a confirmed dead failed session can be dismissed"))
  (let ((agentd-runtime-directory (car agentd-terminal-key)))
    (agentd-dismiss-error
     (cadr agentd-terminal-key)
     (lambda (_result failure)
       (message "%s" (if failure (concat "Dismiss failed: " (plist-get failure :message))
                       "Failure dismissed; terminal output retained"))))))

;;;###autoload
(defun agentd-session-info ()
  "Show this agent's details in the current window, preserving its layout."
  (interactive)
  (agentd-session--check)
  (let ((summary (agentd-session--summary))
        (key agentd-terminal-key) (record agentd-session)
        (directory default-directory) (connection agentd-connection-status)
        (attachment agentd-terminal-state) (persisted agentd-persisted-p)
        (buffer (get-buffer-create "*Agent session*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert summary "\n\n")
        (dolist (entry `(("Session" . ,(cadr key)) ("Runtime" . ,(car key))
                        ("Directory" . ,directory) ("Connection" . ,connection)
                        ("Attachment" . ,attachment) ("State saved to disk" . ,persisted)))
          (insert (format "%s: %s\n" (car entry) (cdr entry))))
        (dolist (field '(:zmx_state :last_event :conversation_id :transcript_path
                        :exit_code :message :attention_agent :observation_error :lifecycle_error))
          (let ((value (plist-get record field)))
            (when (and value (not (eq value :null)))
              (insert (format "%s: %s\n" (substring (symbol-name field) 1) value)))))
        (goto-char (point-min)))
      (special-mode)
      ;; Leaving the details buffer must preserve the window too.
      (use-local-map (copy-keymap special-mode-map))
      (local-set-key (kbd "q") #'bury-buffer))
    (switch-to-buffer buffer)))

(defun agentd-session--restartable-p ()
  (and (memq agentd-kind '(claude codex)) (eq agentd-status 'idle) (not agentd-status-stale-p)
       (not agentd-terminal--killing) (not agentd-terminal-restarting)))

;;;###autoload
(transient-define-prefix agentd-menu ()
  "Launch, find and manage agents from any buffer."
  ["Agents"
   ("n" "New agent…" agentd-new)
   ("b" "Switch agent" agentd-switch-buffer)
   ("B" "Switch agent (all workspaces)" agentd-switch-buffer-all)
   ("j" "Next needing attention" agentd-next-attention)
   ("a" "Attach session…" agentd-attach)
   ("A" "Attach all sessions" agentd-attach-all)
   ("l" "All server sessions…" agentd-overview)]
  [:if agentd-session--buffer-p :description agentd-session--summary
   ("t" "Rename" agentd-rename)
   ("i" "Status and error details" agentd-session-info)
   ("d" "Detach (keep session)" agentd-terminal-detach)
   ("k" "Kill session and buffer" agentd-stop)
   ("r" "Reconnect terminal" agentd-terminal-reconnect)
   ("R" "Resume / redraw history" agentd-terminal-restart :if agentd-session--restartable-p)
   ("e" "Dismiss dead failure" agentd-dismiss-failure :if agentd-session--dismissible-p)]
  (interactive)
  (require 'agentd)
  (transient-setup 'agentd-menu))

(defvar agentd-session-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-a") #'agentd-menu)
    map))

(define-minor-mode agentd-session-mode
  "Current-agent status and controls.  Enabled in agent terminal buffers."
  :lighter (:eval (concat " Agent:" (agentd-session--status)))
  :keymap agentd-session-mode-map
  :group 'agentd)

(provide 'agentd-session)
;;; agentd-session.el ends here
