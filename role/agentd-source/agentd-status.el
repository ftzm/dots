;;; agentd-status.el --- Live metadata on agent buffers -*- lexical-binding: t; -*-
(require 'agentd-client)
(defvar agentd-terminal-key)

(defvar-local agentd-session nil "Latest session record as a plist, or nil if absent.")
(defvar-local agentd-status 'unknown "Agent status: unknown, working, waiting, idle, error or exited.")
(defvar-local agentd-status-stale-p t "Non-nil when the agent status is not current.")
(defvar-local agentd-connection-status 'disconnected "Metadata connection: connecting, connected or disconnected.")
(defvar-local agentd-kind 'unknown "Agent kind: claude, codex or unknown.")
(defvar-local agentd-title nil "User-assigned agent title, or nil.")
(defvar agentd-buffer-update-hook nil
  "Run in an agent buffer after its metadata, cwd or connection state changes.
Read `agentd-status', `agentd-status-stale-p', `agentd-session' and
`default-directory'.  May be made buffer-local.  Terminal attachment state is
separate: a detached buffer still receives agent metadata.")

(cl-defstruct (agentd-status--connection (:constructor agentd-status--make))
  runtime process timer (delay .25) pending ready buffers (persisted t)
  (sessions (make-hash-table :test #'equal)))
(defvar agentd-status--connections (make-hash-table :test #'equal))
(defvar-local agentd-persisted-p t "Whether agentd has saved its current state to disk.")
(defvar-local agentd-status--connection nil)
(defvar-local agentd-status--seen nil)

(defvar agentd-status-health-mode-line '(:eval (agentd-status-health)))
(defun agentd-status-health ()
  "Report unsaved server state across active connections."
  (let (runtimes)
    (maphash (lambda (runtime connection)
               (unless (agentd-status--connection-persisted connection) (push runtime runtimes)))
             agentd-status--connections)
    (when runtimes
      (propertize " [agents: unsaved]" 'face 'warning
                  'help-echo (concat "Agent state has not been saved to disk: " (string-join runtimes ", "))))))

(defun agentd-status--active-p (connection)
  (eq connection (gethash (agentd-status--connection-runtime connection) agentd-status--connections)))

(defun agentd-status--publish (connection)
  "Copy CONNECTION's records into its buffers, then notify integrations."
  (dolist (buffer (copy-sequence (agentd-status--connection-buffers connection)))
    (when (and (buffer-live-p buffer)
               (eq connection (buffer-local-value 'agentd-status--connection buffer)))
      (with-current-buffer buffer
        (let* ((record (gethash (cadr agentd-terminal-key) (agentd-status--connection-sessions connection)))
               (ready (agentd-status--connection-ready connection))
               (record-changed (not (equal record agentd-session)))
               (before (list agentd-session agentd-status agentd-status-stale-p
                             agentd-connection-status default-directory agentd-persisted-p)))
          (setq agentd-persisted-p (agentd-status--connection-persisted connection))
          (setq agentd-connection-status
                (cond (ready 'connected)
                      ((process-live-p (agentd-status--connection-process connection)) 'connecting)
                      (t 'disconnected)))
          (when record
            (setq agentd-status--seen t
                  agentd-session (copy-tree record)
                  agentd-kind (pcase (plist-get record :kind) ("claude" 'claude) ("codex" 'codex) (_ 'unknown))
                  agentd-title (let ((title (plist-get record :title))) (and (stringp title) title))
                  agentd-status (pcase (plist-get record :status)
                                  ("working" 'working) ("waiting" 'waiting) ("idle" 'idle)
                                  ("error" 'error) ("exited" 'exited) (_ 'unknown)))
            (let ((cwd (plist-get record :cwd)))
              (when (and record-changed (stringp cwd) (file-name-absolute-p cwd) (not (file-remote-p cwd)))
                (setq default-directory (file-name-as-directory cwd)))))
          (when (and ready (not record))
            (setq agentd-session nil agentd-status (if agentd-status--seen 'exited 'unknown)))
          (setq agentd-status-stale-p
                (or (not ready)
                    (if record (not (eq (plist-get record :status_stale) :false))
                      (not agentd-status--seen))))
          (unless (equal before (list agentd-session agentd-status agentd-status-stale-p
                                     agentd-connection-status default-directory agentd-persisted-p))
            (condition-case problem
                (run-hooks 'agentd-buffer-update-hook)
              (error (message "Agent buffer update hook: %s" (error-message-string problem))))))))))

(defun agentd-status--cancel-timer (connection)
  (when (agentd-status--connection-timer connection)
    (cancel-timer (agentd-status--connection-timer connection))
    (setf (agentd-status--connection-timer connection) nil)))

(defun agentd-status--close (connection)
  (agentd-status--cancel-timer connection)
  (let ((process (agentd-status--connection-process connection)))
    (setf (agentd-status--connection-process connection) nil
          (agentd-status--connection-ready connection) nil
          (agentd-status--connection-pending connection) "")
    (when (process-live-p process) (delete-process process))))

(defun agentd-status--retry (connection)
  (when (agentd-status--active-p connection)
    (agentd-status--close connection)
    (agentd-status--publish connection)
    (when (agentd-status--active-p connection)
      (let ((delay (agentd-status--connection-delay connection)))
        (setf (agentd-status--connection-delay connection) (min 10 (* 2 delay))
              (agentd-status--connection-timer connection)
              (run-at-time delay nil #'agentd-status--connect connection))))))

(defun agentd-status--validate-record (record)
  (agentd--id (plist-get record :id))
  (dolist (field '(:kind :status :zmx_state))
    (unless (stringp (plist-get record field)) (error "Missing session field %s" field))))

(defun agentd-status--message (connection text)
  (let* ((value (json-parse-string text :object-type 'plist :array-type 'list :null-object :null :false-object :false))
         (type (plist-get value :type)))
    (unless (or (equal type "snapshot") (agentd-status--connection-ready connection))
      (error "Expected initial snapshot"))
    (when (plist-member value :persisted)
      (setf (agentd-status--connection-persisted connection) (eq (plist-get value :persisted) t)))
    (pcase type
      ("snapshot"
       (let ((snapshot (agentd-decode text)) (sessions (make-hash-table :test #'equal)))
         (dolist (record (plist-get snapshot :sessions))
           (puthash (plist-get record :id) record sessions))
         (setf (agentd-status--connection-sessions connection) sessions
               (agentd-status--connection-ready connection) t
               (agentd-status--connection-delay connection) .25)
         (agentd-status--cancel-timer connection)))
      ("update"
       (let ((record (plist-get value :session)))
         (agentd-status--validate-record record)
         (puthash (plist-get record :id) record (agentd-status--connection-sessions connection))))
      ("remove"
       (let ((id (agentd--id (plist-get value :session))))
         (remhash id (agentd-status--connection-sessions connection))
         (dolist (buffer (agentd-status--connection-buffers connection))
           (when (and (buffer-live-p buffer)
                      (equal id (cadr (buffer-local-value 'agentd-terminal-key buffer))))
             (with-current-buffer buffer (setq agentd-status--seen t))))))
      ((or "persistence" "result") nil)
      (_ (error "Unknown agentd stream message: %S" type)))
    (agentd-status--publish connection)))

(defun agentd-status--filter (connection process chunk)
  (when (and (agentd-status--active-p connection)
             (eq process (agentd-status--connection-process connection)))
    (condition-case problem
        (let ((pending (concat (agentd-status--connection-pending connection) chunk)) end)
          (while (setq end (string-match "\n" pending))
            (let ((line (substring pending 0 end)))
              (when (> (string-bytes line) agentd-output-limit) (error "Agentd message too large"))
              (agentd-status--message connection line))
            (setq pending (substring pending (1+ end))))
          (when (> (string-bytes pending) agentd-output-limit) (error "Agentd message too large"))
          (setf (agentd-status--connection-pending connection) pending))
      (error
       (message "Agent status stream: %s" (error-message-string problem))
       (agentd-status--retry connection)))))

(defun agentd-status--connect (connection)
  (when (agentd-status--active-p connection)
    (agentd-status--cancel-timer connection)
    (condition-case nil
        (progn
          (setf (agentd-status--connection-pending connection) ""
                (agentd-status--connection-process connection)
                (make-network-process
                 :name "agentd-status" :family 'local
                 :service (expand-file-name "client.sock" (agentd-status--connection-runtime connection))
                 :coding 'utf-8-unix :noquery t
                 :filter (lambda (process chunk) (agentd-status--filter connection process chunk))
                 :sentinel (lambda (process _event)
                             (when (and (eq process (agentd-status--connection-process connection))
                                        (not (process-live-p process)))
                               (agentd-status--retry connection)))))
          (setf (agentd-status--connection-timer connection)
                (run-at-time 3 nil (lambda ()
                                     (unless (agentd-status--connection-ready connection)
                                       (agentd-status--retry connection)))))
          (agentd-status--publish connection))
      (error (agentd-status--retry connection)))))

(defun agentd-status--unwatch ()
  (when-let* ((connection agentd-status--connection))
    (setq agentd-status--connection nil)
    (setf (agentd-status--connection-buffers connection)
          (delq (current-buffer) (agentd-status--connection-buffers connection)))
    (unless (agentd-status--connection-buffers connection)
      (remhash (agentd-status--connection-runtime connection) agentd-status--connections)
      (agentd-status--close connection))))

(defun agentd-status-watch ()
  "Keep this agent buffer's metadata and `default-directory' up to date."
  (unless agentd-status--connection
    (let* ((runtime (agentd--local-directory (car agentd-terminal-key)))
           (connection (or (gethash runtime agentd-status--connections)
                           (let ((new (agentd-status--make :runtime runtime)))
                             (puthash runtime new agentd-status--connections) new))))
      (setq agentd-status--connection connection)
      (cl-pushnew (current-buffer) (agentd-status--connection-buffers connection))
      (add-hook 'kill-buffer-hook #'agentd-status--unwatch nil t)
      (add-hook 'change-major-mode-hook #'agentd-status--unwatch nil t)
      (if (or (process-live-p (agentd-status--connection-process connection))
              (agentd-status--connection-timer connection))
          (agentd-status--publish connection)
        (agentd-status--connect connection)))))

(defun agentd-buffer-waiting-p (buffer)
  "Return non-nil if BUFFER has a current waiting agent status."
  (and (buffer-live-p buffer)
       (eq (buffer-local-value 'agentd-status buffer) 'waiting)
       (eq (buffer-local-value 'agentd-connection-status buffer) 'connected)
       (not (buffer-local-value 'agentd-status-stale-p buffer))))

(cl-defun agentd-waiting-buffers (&optional (buffers (buffer-list)))
  "Return waiting agent buffers from BUFFERS, preserving their order.
Pass the current perspective/workspace's buffer list to restrict the result.
Omitting BUFFERS uses all buffers; explicitly passing nil returns nil."
  (cl-remove-if-not #'agentd-buffer-waiting-p buffers))

(provide 'agentd-status)
;;; agentd-status.el ends here
