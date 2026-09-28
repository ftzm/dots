;;; agentd-client.el --- Programmatic agent CLI interface -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defgroup agentd nil "Local persistent agents." :group 'processes)
(defconst agentd--directory (file-name-directory (or load-file-name buffer-file-name)))
(defcustom agentd-control-program "agentctl"
  "Agent control executable, resolved through `exec-path' unless absolute." :type 'string)
(defcustom agentd-launch-program "agent-new"
  "Harness launcher executable, resolved through `exec-path' unless absolute." :type 'string)
(defcustom agentd-runtime-directory nil
  "Local runtime directory, or nil for the environment default." :type '(choice directory (const nil)))
(defcustom agentd-request-timeout 8 "Metadata command deadline in seconds." :type 'number)
(defcustom agentd-output-limit (* 4 1024 1024) "Maximum metadata output bytes." :type 'integer)

(defun agentd--local-directory (path)
  (unless (and (stringp path) (file-name-absolute-p path) (not (file-remote-p path)))
    (error "Expected an absolute local directory: %S" path))
  (directory-file-name (expand-file-name path)))

(defun agentd-runtime ()
  "Return the absolute local runtime directory for this connection."
  (agentd--local-directory
   (or agentd-runtime-directory (getenv "AGENTD_RUNTIME_DIR")
       (when-let* ((root (getenv "XDG_RUNTIME_DIR"))) (concat root "/agentd")))))

(defun agentd--id (id)
  (unless (and (stringp id) (string-match-p "\\`[a-zA-Z0-9_-]\\{1,128\\}\\'" id))
    (error "Invalid session ID: %S" id))
  id)

(defun agentd-decode (text)
  "Decode one CLI JSON object as a plist.
JSON null is :null, false is :false, arrays are lists, enums remain strings.
Use `plist-member' to distinguish an absent field from a present null."
  (let ((value (json-parse-string text :object-type 'plist :array-type 'list
                                  :null-object :null :false-object :false)))
    (unless (and (listp value) (member (plist-get value :type) '("snapshot" "result")))
      (error "Expected snapshot or result"))
    (unless (memq (plist-get value :persisted) '(t :false)) (error "Missing persisted flag"))
    (if (equal (plist-get value :type) "snapshot")
        (progn
          (unless (and (equal (plist-get value :version) 1)
                       (plist-member value :sessions) (listp (plist-get value :sessions)))
            (error "Invalid snapshot"))
          (dolist (session (plist-get value :sessions))
            (agentd--id (plist-get session :id))
            (dolist (field '(:kind :status :zmx_state))
              (unless (stringp (plist-get session field)) (error "Missing session field %s" field)))))
      (dolist (field '(:ok :applied))
        (unless (memq (plist-get value field) '(t :false)) (error "Missing result field %s" field))))
    value))

(defun agentd--arguments (request)
  "Build metadata argv from REQUEST, a plist with :op and operation fields."
  (let ((op (plist-get request :op)) (id (plist-get request :session)))
    (cons "--json"
          (cond
           ((equal op "list") '("list"))
           ((member op '("kill" "dismiss_error" "restart")) (list op (agentd--id id)))
           ((equal op "set_title")
            (let ((title (plist-get request :title)))
              (cond ((eq title :null) (list "clear-title" (agentd--id id)))
                    ((stringp title) (list "title" (agentd--id id) title))
                    (t (error "Title must be a string or :null")))))
           (t (error "Unknown metadata operation: %S" op))))))

(defun agentd-request (request callback)
  "Run REQUEST asynchronously; call CALLBACK with (RESULT ERROR).
ERROR is nil or a plist with :kind and :message.  Server rejection also
returns its RESULT.  Applied-but-unpersisted results remain successful.
Return a cancellation function.  Never retry a mutation."
  (let* ((args (agentd--arguments request))
         (runtime (agentd-runtime))
         (default-directory "/")
         (process-environment (cons (concat "AGENTD_RUNTIME_DIR=" runtime) process-environment))
         (out "") (err "") done process stderr-process timer
         finish)
    (setq finish
          (lambda (failure)
            (unless done
              (setq done t)
              (when timer (cancel-timer timer))
              (when (process-live-p process) (delete-process process))
              (when (process-live-p stderr-process) (delete-process stderr-process))
              (let (result)
                (unless failure
                  (condition-case problem
                      (setq result (agentd-decode out))
                    (error (setq failure
                                 (list :kind (if (zerop (process-exit-status process)) 'protocol 'cli)
                                       :message (if (and (not (zerop (process-exit-status process)))
                                                         (not (string-empty-p (string-trim err))))
                                                    (string-trim err)
                                                  (concat (error-message-string problem) ": " err)))))))
                (when (and result (equal (plist-get result :type) "result")
                           (eq (plist-get result :ok) :false))
                  (setq failure (list :kind 'server :code (plist-get result :error_code) :message (plist-get result :error))))
                (when (and (not failure) (not (zerop (process-exit-status process))))
                  (setq failure (list :kind 'cli :message err)))
                (funcall callback result failure)))))
    (condition-case problem
        (progn
          (setq stderr-process
                (make-pipe-process
                 :name "agentd-stderr" :noquery t :coding 'utf-8-unix
                 :filter (lambda (_ chunk)
                           (setq err (concat err chunk))
                           (when (> (string-bytes err) agentd-output-limit)
                             (funcall finish '(:kind limit :message "stderr limit exceeded"))))))
          (setq process
                (make-process
                 :name "agentd-request" :command (cons agentd-control-program args)
                 :connection-type 'pipe :coding 'utf-8-unix :noquery t :stderr stderr-process
                 :filter (lambda (_ chunk)
                           (setq out (concat out chunk))
                           (when (> (string-bytes out) agentd-output-limit)
                             (funcall finish '(:kind limit :message "stdout limit exceeded"))))
                 :sentinel (lambda (p _event)
                             (when (memq (process-status p) '(exit signal failed)) (funcall finish nil)))))
          (setq timer (run-at-time agentd-request-timeout nil finish
                                   '(:kind timeout :message "agentctl timed out"))))
      (error (funcall finish (list :kind 'transport :message (error-message-string problem)))))
    (lambda () (funcall finish '(:kind cancelled :message "Request cancelled")))))

(defun agentd-list (callback) (agentd-request '(:op "list") callback))
(defun agentd-set-title (id title callback)
  (agentd-request (list :op "set_title" :session id :title title) callback))
(defun agentd-kill (id callback)
  (agentd-request (list :op "kill" :session id) callback))
(defun agentd-dismiss-error (id callback)
  (agentd-request (list :op "dismiss_error" :session id) callback))
(defun agentd-restart (id callback)
  "Explicitly restart idle Claude or Codex ID and resume its conversation."
  (let ((agentd-request-timeout 45))
    (agentd-request (list :op "restart" :session id) callback)))

(defun agentd-launch-description (kind directory &optional arguments title)
  "Return a PTY launch description, including its preallocated UUID.
KIND is claude or codex as a string. ARGUMENTS is a list of argv strings.
The description is intent, not confirmation that a harness has started."
  (unless (member kind '("claude" "codex")) (error "Unknown harness: %S" kind))
  (unless (and (listp arguments) (cl-every #'stringp arguments)) (error "Arguments must be strings"))
  (let ((directory (agentd--local-directory directory))
        (id (with-temp-buffer (insert-file-contents-literally "/proc/sys/kernel/random/uuid")
                              (string-trim (buffer-string)))))
    (list :id id :runtime (agentd-runtime) :directory directory :program agentd-launch-program
          :title title
          :args (append (list "--id" id) (when title (list "--title" title))
                        (list kind directory "--") arguments))))

(defun agentd-attach-description (id &optional directory)
  "Return a PTY attachment description for ID."
  (list :id (agentd--id id) :runtime (agentd-runtime)
        :directory (agentd--local-directory (or directory "/"))
        :program agentd-control-program :args (list "attach" id)))

(provide 'agentd-client)
;;; agentd-client.el ends here
