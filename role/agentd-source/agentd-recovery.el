;;; agentd-recovery.el --- Restore agent buffers and workspace membership -*- lexical-binding: t; -*-
(require 'agentd-terminal)

(defcustom agentd-recover-on-start t
  "Restore live agent buffers when `agentd-mode' is enabled."
  :type 'boolean :group 'agentd)
(defcustom agentd-recovery-file (locate-user-emacs-file "agentd-workspaces.json")
  "File containing agent workspace memberships and explicit detach choices."
  :type 'file :group 'agentd)
(defvar agentd-recovery--records nil)
(defvar agentd-recovery--loaded nil)
(defvar agentd-recovery--load-error nil)
(defvar agentd-recovery--timer nil)
(defvar agentd-recovery--restoring nil)
(defvar agentd-recovery--retry nil)
(defvar agentd-mode)
(defvar agentd-recovery-memberships-function nil
  "Optional function returning the current buffer's workspace names.")
(defvar agentd-recovery-restore-function nil
  "Optional function called with BUFFER and saved workspace names.")

(defun agentd-recovery--load ()
  (unless agentd-recovery--loaded
    (setq agentd-recovery--loaded t)
    (when (file-exists-p agentd-recovery-file)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents agentd-recovery-file)
            (let ((rows (json-parse-buffer :object-type 'plist :array-type 'list)))
              (unless (cl-every (lambda (r)
                                  (and (stringp (plist-get r :runtime))
                                       (stringp (plist-get r :id))
                                       (listp (plist-get r :workspaces))
                                       (cl-every #'stringp (plist-get r :workspaces)))) rows)
                (error "Invalid workspace records"))
              (setq agentd-recovery--records rows)))
        (error (setq agentd-recovery--load-error t)
               (message "Agent workspace recovery: %s" (error-message-string err)))))))

(defun agentd-recovery--record (key)
  (cl-find-if (lambda (r) (equal key (list (plist-get r :runtime) (plist-get r :id))))
              agentd-recovery--records))

(defun agentd-recovery-save ()
  "Atomically save memberships, including those shared by several workspaces."
  (when agentd-recovery--timer (cancel-timer agentd-recovery--timer))
  (setq agentd-recovery--timer nil)
  (unless agentd-recovery--restoring
    (agentd-recovery--load)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when agentd-terminal-key
          (let ((record (or (agentd-recovery--record agentd-terminal-key)
                            (let ((new (list :runtime (car agentd-terminal-key)
                                             :id (cadr agentd-terminal-key) :detached :false :workspaces nil)))
                              (push new agentd-recovery--records) new))))
            (setf (plist-get record :detached) :false)
            (when agentd-recovery-memberships-function
              (setf (plist-get record :workspaces)
                    (funcall agentd-recovery-memberships-function)))))))
    (unless agentd-recovery--load-error
      (condition-case err
          (let* ((directory (file-name-directory agentd-recovery-file)) temporary)
            (make-directory directory t)
            (unwind-protect
		(progn
                  (setq temporary (make-temp-file (expand-file-name ".agentd-" directory)))
                  (with-temp-file temporary
                    (insert (json-serialize
                             (vconcat (mapcar (lambda (r)
						(let ((copy (copy-sequence r)))
                                                  (setf (plist-get copy :workspaces)
							(vconcat (plist-get r :workspaces))) copy))
                                              agentd-recovery--records)))))
                  (rename-file temporary agentd-recovery-file t))
              (when (and temporary (file-exists-p temporary)) (delete-file temporary))))
	(error (message "Cannot save agent workspaces: %s" (error-message-string err)))))))

(defun agentd-recovery-schedule (&rest _)
  (when (and (bound-and-true-p agentd-mode) (not agentd-recovery--restoring)
             (not agentd-recovery--timer))
    (setq agentd-recovery--timer (run-at-time .2 nil #'agentd-recovery-save))))

(defun agentd-recovery--detached ()
  (agentd-recovery-save)
  (when-let* ((record (agentd-recovery--record agentd-terminal-key)))
    (setf (plist-get record :detached) t)
    ;; Write after the buffer closes, without recapturing it as attached.
    (let ((agentd-terminal-key nil)) (agentd-recovery-save))))

(defun agentd-recover ()
  "Restore live sessions without selecting buffers or changing the layout.
Explicitly detached sessions remain detached. Unknown external sessions are
attached in the background; they are available in the all-workspace picker."
  (interactive)
  (when agentd-recovery--retry (cancel-timer agentd-recovery--retry))
  (setq agentd-recovery--retry nil)
  (agentd-recovery--load)
  (let ((runtime (agentd-runtime)))
    (agentd-list
     (lambda (snapshot failure)
       (when (bound-and-true-p agentd-mode)
         (if failure
             (progn
               (message "Agent recovery: %s; retrying" (plist-get failure :message))
               (setq agentd-recovery--retry (run-at-time 5 nil #'agentd-recover)))
           (let ((agentd-runtime-directory runtime)
                 (agentd-terminal-background t)
                 (agentd-recovery--restoring t)
                 retry)
             (dolist (session (plist-get snapshot :sessions))
               (let* ((id (plist-get session :id)) (key (list runtime id))
                      (saved (agentd-recovery--record key)))
                 (when (equal (plist-get session :zmx_state) "unknown") (setq retry t))
                 (when (and (equal (plist-get session :zmx_state) "alive")
                            (not (eq (plist-get saved :detached) t))
                            (not (buffer-live-p (gethash key agentd-terminal-buffers))))
                   (condition-case err
                       (let ((buffer (agentd-terminal-attach id (let ((cwd (plist-get session :cwd))) (when (stringp cwd) cwd)))))
                         (when (and saved agentd-recovery-restore-function)
                           (funcall agentd-recovery-restore-function buffer (plist-get saved :workspaces))))
                     (error (setq retry t)
                            (message "Restore agent %s: %s" id (error-message-string err)))))))
             (when retry
               (setq agentd-recovery--retry (run-at-time 5 nil #'agentd-recover))))
           (agentd-recovery-save)))))))

(defun agentd-recovery-enable ()
  (add-hook 'agentd-terminal-open-hook #'agentd-recovery-schedule)
  (add-hook 'agentd-terminal-detach-hook #'agentd-recovery--detached)
  (add-hook 'kill-emacs-hook #'agentd-recovery-save)
  (when (and agentd-recover-on-start (not noninteractive))
    (setq agentd-recovery--retry (run-at-time 0 nil #'agentd-recover))))

(defun agentd-recovery-disable ()
  (dolist (timer (list agentd-recovery--retry agentd-recovery--timer))
    (when timer (cancel-timer timer)))
  (setq agentd-recovery--retry nil agentd-recovery--timer nil)
  (remove-hook 'agentd-terminal-open-hook #'agentd-recovery-schedule)
  (remove-hook 'agentd-terminal-detach-hook #'agentd-recovery--detached)
  (remove-hook 'kill-emacs-hook #'agentd-recovery-save))

(provide 'agentd-recovery)
