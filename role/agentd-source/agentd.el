;;; agentd.el --- Persistent coding agents in Ghostel -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (ghostel "0.52.0") (marginalia "2.0"))
;; Keywords: processes, terminals, tools

;;; Commentary:
;; Install the separate agentd runtime on PATH, then enable `agentd-mode'.
;; `agentd-new' launches Claude or Codex; `agentd-attach' and
;; `agentd-attach-all' recover existing sessions.  Agents survive Emacs exit.
;; Hooks are configured per launch without changing harness dotfiles.

;;; Code:
(require 'agentd-terminal)
(require 'agentd-launch)
(require 'agentd-session)
(require 'agentd-recovery)

;;;###autoload
(define-minor-mode agentd-mode
  "Enable live agent metadata, attention display and buffer navigation.
Disabling stops subscriptions and the attention display.  It does not kill
agents, close their terminal buffers, or detach their terminal processes."
  :global t :group 'agentd
  (if agentd-mode
      (progn
        (agentd-attention-mode 1)
        (add-to-list 'global-mode-string agentd-status-health-mode-line t)
        (agentd-recovery-enable)
        (maphash (lambda (_key buffer)
                   (when (buffer-live-p buffer)
                     (with-current-buffer buffer (agentd-status-watch))))
                 agentd-terminal-buffers))
    (setq global-mode-string (delete agentd-status-health-mode-line global-mode-string))
    (agentd-recovery-disable)
    (agentd-attention-mode -1)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when agentd-status--connection
          (agentd-status--unwatch)
          (setq agentd-status-stale-p t agentd-connection-status 'disconnected))))))

;;;###autoload
(defun agentd-new (&optional kind directory arguments title)
  "Launch KIND in DIRECTORY, with optional harness argv ARGUMENTS.
Interactively, open the launch panel.  Programmatic calls launch directly;
optional TITLE names the session and its buffer."
  (interactive)
  (if (null kind)
      (call-interactively #'agentd-launch)
    (unless agentd-mode (agentd-mode 1))
    (agentd-terminal-new kind directory arguments title)))

(defun agentd--attach-sessions (all)
  "Fetch live sessions and attach all if ALL, otherwise prompt for one."
  (let ((runtime (agentd-runtime)))
    (agentd-list
     (lambda (snapshot failure)
       (if failure
           (message "Agent sessions: %s" (plist-get failure :message))
         (let ((sessions (cl-remove-if-not
                          (lambda (session) (equal (plist-get session :zmx_state) "alive"))
                          (plist-get snapshot :sessions))))
           (if (not sessions)
               (message "No live agent sessions")
             (unless all
               (let* ((choices (mapcar
                                (lambda (session)
                                  (cons (format "%s  %s  %s"
                                                (plist-get session :id)
                                                (plist-get session :kind)
                                                (let ((title (plist-get session :title)))
                                                  (if (stringp title) title ""))) session))
                                sessions))
                      (choice (completing-read "Attach agent: " choices nil t)))
                 (setq sessions (list (cdr (assoc choice choices))))))
             (let ((agentd-runtime-directory runtime))
               (dolist (session sessions)
                 (condition-case problem
                     (agentd-terminal-attach
                      (plist-get session :id)
                      (let ((cwd (plist-get session :cwd))) (when (stringp cwd) cwd)))
                   (error (message "Attach %s: %s" (plist-get session :id)
                                   (error-message-string problem)))))))))))))

;;;###autoload
(defun agentd-attach ()
  "Choose a live session and attach it in Ghostel."
  (interactive)
  (unless agentd-mode (agentd-mode 1))
  (agentd--attach-sessions nil))

;;;###autoload
(defun agentd-attach-all ()
  "Attach all live sessions, reusing existing agent buffers."
  (interactive)
  (unless agentd-mode (agentd-mode 1))
  (agentd--attach-sessions t))

(provide 'agentd)
;;; agentd.el ends here
