;;; agentd-overview.el --- Browse every daemon session -*- lexical-binding: t; -*-
(require 'agentd-terminal)
(require 'tabulated-list)
(defvar-local agentd-overview--runtime nil)
(defvar-local agentd-overview--records nil)
(defvar-local agentd-overview--timer nil)
(defvar-local agentd-overview--pending nil)

(defun agentd-overview-refresh ()
  "Refresh live, detached and dead sessions from agentd."
  (interactive)
  (unless agentd-overview--pending
    (setq agentd-overview--pending t)
    (let ((buffer (current-buffer)) (agentd-runtime-directory agentd-overview--runtime))
      (agentd-list
       (lambda (snapshot failure)
	 (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (setq agentd-overview--pending nil)
             (if failure
		 (setq header-line-format (concat "Disconnected: " (plist-get failure :message)))
               (setq header-line-format (if (eq (plist-get snapshot :persisted) t)
                                            "RET attach · i details · t rename · k kill · e dismiss failure · g refresh"
                                          (propertize "State NOT SAVED to disk — g refresh, i details" 'face 'warning))
                     agentd-overview--records (plist-get snapshot :sessions)
                     tabulated-list-entries
                     (mapcar (lambda (s)
                               (list (plist-get s :id)
                                     (vector (if (stringp (plist-get s :title)) (plist-get s :title) (plist-get s :id))
                                             (plist-get s :kind) (plist-get s :status)
                                             (plist-get s :zmx_state)
                                             (if (stringp (plist-get s :cwd)) (plist-get s :cwd) ""))))
                             agentd-overview--records))
               (tabulated-list-print t)))))))))

(defun agentd-overview--record ()
  (or (cl-find (tabulated-list-get-id) agentd-overview--records
               :key (lambda (s) (plist-get s :id)) :test #'equal)
      (user-error "No session on this row")))

(defun agentd-overview-attach ()
  (interactive)
  (let* ((s (agentd-overview--record)) (agentd-runtime-directory agentd-overview--runtime)
         (buffer (gethash (list agentd-overview--runtime (plist-get s :id)) agentd-terminal-buffers)))
    (if (buffer-live-p buffer)
        (funcall agentd-attention-switch-buffer-function buffer)
      (unless (equal (plist-get s :zmx_state) "alive") (user-error "Session is not confirmed alive; use i for details"))
      (agentd-terminal-attach (plist-get s :id) (let ((cwd (plist-get s :cwd))) (when (stringp cwd) cwd))))))

(defun agentd-overview-details ()
  (interactive)
  (let ((record (agentd-overview--record)) (buffer (get-buffer-create "*Agent record*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (pp-to-string record))
        (goto-char (point-min)))
      (special-mode)
      (use-local-map (copy-keymap special-mode-map))
      (local-set-key (kbd "q") #'bury-buffer))
    (switch-to-buffer buffer)))

(defun agentd-overview--mutate (op &optional title)
  (let* ((s (agentd-overview--record)) (buffer (current-buffer))
         (agentd-runtime-directory agentd-overview--runtime)
         (request (list :op op :session (plist-get s :id))))
    (when title (setq request (append request (list :title title))))
    (agentd-request request
                    (lambda (_ failure)
                      (if failure (message "Agent: %s" (plist-get failure :message))
                        (when (buffer-live-p buffer)
                          (with-current-buffer buffer (agentd-overview-refresh))))))))
(defun agentd-overview-kill ()
  (interactive)
  (when (yes-or-no-p "Kill this agent session? ") (agentd-overview--mutate "kill")))
(defun agentd-overview-dismiss ()
  (interactive)
  (agentd-overview--mutate "dismiss_error"))
(defun agentd-overview-rename (title)
  (interactive "sAgent title: ")
  (agentd-terminal-check-name title (gethash (list agentd-overview--runtime (tabulated-list-get-id)) agentd-terminal-buffers))
  (agentd-overview--mutate "set_title" title))

(defun agentd-overview--stop ()
  (when agentd-overview--timer (cancel-timer agentd-overview--timer))
  (setq agentd-overview--timer nil))

(define-derived-mode agentd-overview-mode tabulated-list-mode "Agents"
  "All daemon records, including sessions with no Emacs buffer."
  (setq tabulated-list-format [("Title / ID" 28 t) ("Harness" 9 t) ("Status" 10 t) ("Process" 9 t) ("Directory" 0 t)])
  (setq tabulated-list-padding 1)
  (add-hook 'tabulated-list-revert-hook #'agentd-overview-refresh nil t)
  (add-hook 'kill-buffer-hook #'agentd-overview--stop nil t)
  (add-hook 'change-major-mode-hook #'agentd-overview--stop nil t)
  (tabulated-list-init-header))
(dolist (entry '(("RET" . agentd-overview-attach) ("i" . agentd-overview-details)
                 ("k" . agentd-overview-kill) ("e" . agentd-overview-dismiss)
                 ("t" . agentd-overview-rename) ("g" . agentd-overview-refresh) ("q" . bury-buffer)))
  (define-key agentd-overview-mode-map (kbd (car entry)) (cdr entry)))

;;;###autoload
(defun agentd-overview ()
  "Show all server sessions in the current window."
  (interactive)
  (let ((runtime (agentd-runtime)) (buffer (get-buffer-create "*Agents*")))
    (with-current-buffer buffer
      (agentd-overview-mode)
      (setq agentd-overview--runtime runtime)
      (agentd-overview-refresh)
      (setq agentd-overview--timer
            (run-at-time 1 1 (lambda ()
                               (when (and (buffer-live-p buffer) (get-buffer-window buffer t))
                                 (with-current-buffer buffer (agentd-overview-refresh)))))))
    (switch-to-buffer buffer)))
(provide 'agentd-overview)
