;;; agentd-launch.el --- Project and worktree launch panel -*- lexical-binding: t; -*-

(require 'agentd-terminal)
(require 'project)
(require 'transient)

(declare-function agentd-new "agentd" (&optional kind directory arguments title))
(defcustom agentd-launch-harnesses
  '(("Claude" :kind "claude" :arguments nil)
    ("Codex" :kind "codex" :arguments nil))
  "Named launch presets for supported harnesses, with optional argv defaults."
  :type '(alist :key-type string :value-type plist) :group 'agentd)
(defcustom agentd-launch-default-harness "Claude"
  "Initial preset before a successful launch from the panel."
  :type 'string :group 'agentd)
(defvar agentd-launch--last-harness nil)
(defvar agentd-launch--state nil "Draft belonging to the current launch panel.")
(defvar-local agentd-launch--pending-harness nil)

(defun agentd-launch--git (directory &rest args)
  "Return Git output in DIRECTORY, or nil on failure."
  (let ((default-directory (file-name-as-directory directory)))
    (with-temp-buffer
      (when (zerop (apply #'process-file "git" nil (list t nil) nil args))
        (buffer-string)))))

(defun agentd-launch--git-required (directory &rest args)
  "Run Git with ARGS in DIRECTORY, reporting errors."
  (let ((default-directory (file-name-as-directory directory)))
    (with-temp-buffer
      (unless (zerop (apply #'process-file "git" nil t nil args))
        (user-error "Git: %s" (string-trim (buffer-string))))
      (buffer-string))))

(defun agentd-launch--worktrees (directory)
  "Return worktree plists in DIRECTORY's repository, main checkout first."
  (when-let* ((output (agentd-launch--git directory "worktree" "list" "--porcelain" "-z")))
    (mapcar
     (lambda (record)
       (let (result)
         (dolist (field (split-string record "\0" t))
           (cond ((string-prefix-p "worktree " field)
                  (setq result (plist-put result :directory (substring field 9))))
                 ((string-prefix-p "branch refs/heads/" field)
                  (setq result (plist-put result :branch (substring field 18))))
                 ((equal field "bare") (setq result (plist-put result :bare t)))))
         result))
     (split-string output "\0\0" t))))

(defun agentd-launch--location (directory)
  "Resolve DIRECTORY without replacing a deliberately selected subdirectory."
  (setq directory (file-name-as-directory (agentd--local-directory directory)))
  (unless (file-directory-p directory) (user-error "No such directory: %s" directory))
  (let* ((top (and (executable-find "git")
                   (agentd-launch--git directory "rev-parse" "--show-toplevel")))
         (top (and top (file-name-as-directory (string-trim-right top "\n"))))
         (trees (and top (agentd-launch--worktrees directory)))
         (main (plist-get (car trees) :directory))
         (tree (and top (cl-find-if
                        (lambda (entry) (file-equal-p top (plist-get entry :directory))) trees))))
    (if main
        (list :directory directory :project (file-name-as-directory main) :git t
              :root top :worktree (if (file-equal-p top main) "main checkout"
                                   (or (plist-get tree :branch)
                                       (file-name-nondirectory (directory-file-name top)))))
      (let ((project (project-current nil directory)))
        (list :directory directory :project (and project (project-root project)))))))

(defun agentd-launch--unused-name (base)
  (let ((name base) (number 2))
    (while (agentd-terminal-name-used-p name)
      (setq name (format "%s-%d" base number) number (1+ number)))
    name))

(defun agentd-launch--set-directory (state directory)
  "Update STATE's location and its automatic name from DIRECTORY."
  (let ((location (agentd-launch--location directory)))
    (dolist (key '(:directory :project :git :root :worktree))
      (setf (plist-get state key) (plist-get location key)))
    (setf (plist-get state :new-worktree) nil)
    (unless (plist-get state :name-edited)
      (setf (plist-get state :name)
            (agentd-launch--unused-name
             (or (let ((base (file-name-nondirectory
                              (directory-file-name
                               (or (plist-get state :root) directory)))))
                   (unless (string-empty-p base) base)) "agent")))))
  state)

(defun agentd-launch--initial-state (directory)
  (let* ((harness (or (and (assoc agentd-launch--last-harness agentd-launch-harnesses)
                           agentd-launch--last-harness)
                      (and (assoc agentd-launch-default-harness agentd-launch-harnesses)
                           agentd-launch-default-harness)
                      (caar agentd-launch-harnesses)))
         (state (list :harness harness :runtime (agentd-runtime)
                      :directory nil :project nil :git nil :root nil :worktree nil
                      :name nil :name-edited nil :new-worktree nil
                      :arguments (copy-sequence
                                  (plist-get (cdr (assoc harness agentd-launch-harnesses)) :arguments)))))
    (unless harness (user-error "No harness presets configured"))
    (agentd-launch--set-directory state directory)
    ;; Start at the current checkout root, never the owning main checkout.
    (when-let* ((root (plist-get state :root)))
      (agentd-launch--set-directory state root))
    state))

(defun agentd-launch--field (label key)
  (format "%-10s %s" label (or (plist-get agentd-launch--state key) "—")))
(defun agentd-launch--git-p () (plist-get agentd-launch--state :git))
(defun agentd-launch--directory-label ()
  (concat (agentd-launch--field "Directory" :directory)
          (when (plist-get agentd-launch--state :new-worktree) " (create on launch)")))

(defun agentd-launch-harness ()
  "Select a harness preset."
  (interactive)
  (let ((choice (completing-read "Harness: " agentd-launch-harnesses nil t nil nil
                                  (plist-get agentd-launch--state :harness))))
    (setf (plist-get agentd-launch--state :harness) choice
          (plist-get agentd-launch--state :arguments)
          (copy-sequence (plist-get (cdr (assoc choice agentd-launch-harnesses)) :arguments)))))

(defun agentd-launch-name ()
  "Set a free-text session name."
  (interactive)
  (setf (plist-get agentd-launch--state :name)
        (read-string "Session name: " (plist-get agentd-launch--state :name))
        (plist-get agentd-launch--state :name-edited) t))

(defun agentd-launch-directory ()
  "Choose an exact launch directory and infer project and worktree."
  (interactive)
  (agentd-launch--set-directory
   agentd-launch--state
   (read-directory-name "Launch directory: " (plist-get agentd-launch--state :directory) nil t)))

(defun agentd-launch-project ()
  "Choose a project, returning to this panel at its main checkout."
  (interactive)
  (let* ((directory (project-prompt-project-dir))
         (location (agentd-launch--location directory)))
    (agentd-launch--set-directory
     agentd-launch--state (or (plist-get location :project) directory))))

(defun agentd-launch-worktree ()
  "Choose an existing worktree or configure a new one."
  (interactive)
  (unless (agentd-launch--git-p) (user-error "Select a Git project first"))
  (let* ((trees (cl-remove-if (lambda (entry) (plist-get entry :bare))
                             (agentd-launch--worktrees (plist-get agentd-launch--state :project))))
         (choices (mapcar
                   (lambda (tree)
                     (cons (format "%s  %s" (or (plist-get tree :branch) "detached")
                                   (plist-get tree :directory))
                           (plist-get tree :directory))) trees))
         (choice (completing-read "Worktree: " (append choices '(("Create new worktree…"))) nil t)))
    (if (equal choice "Create new worktree…") (agentd-launch-create-worktree)
      (agentd-launch--set-directory agentd-launch--state (cdr (assoc choice choices))))))

(defun agentd-launch--branch-suggestion (name)
  "Turn free text NAME into an editable conservative branch suggestion."
  (let ((name (replace-regexp-in-string "[^[:alnum:]_-]+" "-" name)))
    (string-trim name "-+" "-+")))

(defun agentd-launch--plan-worktree (state branch directory start)
  "Validate and record a new worktree in STATE without creating anything."
  (let ((project (plist-get state :project)))
    (unless (plist-get state :git) (user-error "Select a Git project first"))
    (unless (and (not (string-prefix-p "-" branch))
                 (agentd-launch--git project "check-ref-format" (concat "refs/heads/" branch)))
      (user-error "Invalid branch name: %s" branch))
    (when (agentd-launch--git project "show-ref" "--verify" (concat "refs/heads/" branch))
      (user-error "Branch already exists: %s" branch))
    (agentd-launch--git-required project "rev-parse" "--verify" "--end-of-options" (concat start "^{commit}"))
    (setq directory (agentd--local-directory directory))
    (when (file-exists-p directory) (user-error "Worktree path already exists: %s" directory))
    (setf (plist-get state :new-worktree) (list :branch branch :directory directory :start start)
          (plist-get state :worktree) branch
          (plist-get state :directory) (file-name-as-directory directory)
          (plist-get state :name) branch
          (plist-get state :name-edited) t)))

(defun agentd-launch-create-worktree ()
  "Configure a new branch and worktree; defer creation until Launch."
  (interactive)
  (unless (agentd-launch--git-p) (user-error "Select a Git project first"))
  (let* ((project (plist-get agentd-launch--state :project))
         (branch (read-string "New worktree / branch name: "
                              (agentd-launch--branch-suggestion (plist-get agentd-launch--state :name))))
         (path (read-directory-name
                "New worktree directory: " (file-name-directory (directory-file-name project))
                nil nil (concat (file-name-nondirectory (directory-file-name project)) "-"
                                (replace-regexp-in-string "/" "-" branch))))
         (start (read-string "Start from branch or commit: " "HEAD")))
    (agentd-launch--plan-worktree agentd-launch--state branch path start)))

(defun agentd-launch-arguments ()
  "Edit additional harness arguments as argv, without shell evaluation."
  (interactive)
  (setf (plist-get agentd-launch--state :arguments)
        (split-string-and-unquote
         (read-string "Harness arguments: "
                      (combine-and-quote-strings (plist-get agentd-launch--state :arguments))))))

(defun agentd-launch--remember-harness ()
  (when (and agentd-launch--pending-harness
             (equal (plist-get agentd-session :zmx_state) "alive")
             (not (eq agentd-status 'exited)))
    (setq agentd-launch--last-harness agentd-launch--pending-harness
          agentd-launch--pending-harness nil)
    (remove-hook 'agentd-buffer-update-hook #'agentd-launch--remember-harness t)))

(defun agentd-launch--validate-name (name)
  (agentd-terminal-check-name name))

(defun agentd-launch--problem ()
  (condition-case problem
      (progn (agentd-launch--validate-name (plist-get agentd-launch--state :name)) nil)
    (user-error (error-message-string problem))))

(defun agentd-launch--launch-label ()
  (if-let* ((problem (agentd-launch--problem))) (concat "Launch — " problem) "Launch"))

(defun agentd-launch-start ()
  "Create any pending worktree and launch the displayed session."
  (interactive)
  (let* ((state agentd-launch--state)
         (preset (cdr (assoc (plist-get state :harness) agentd-launch-harnesses)))
         (kind (plist-get preset :kind))
         (name (plist-get state :name))
         (agentd-runtime-directory (plist-get state :runtime)))
    (agentd-launch--validate-name name)
    (unless (member kind '("claude" "codex")) (user-error "Unsupported harness: %s" kind))
    (unless (executable-find agentd-launch-program) (user-error "Cannot find %s" agentd-launch-program))
    (when-let* ((pending (plist-get state :new-worktree)))
      (agentd-launch--git-required
       (plist-get state :project) "worktree" "add" "-b" (plist-get pending :branch)
       "--" (plist-get pending :directory) (plist-get pending :start))
      ;; Keep a successfully created worktree on later launch failure. Retrying
      ;; launches into it instead of trying to create the branch a second time.
      (agentd-launch--set-directory state (plist-get pending :directory)))
    (unless (file-directory-p (plist-get state :directory)) (user-error "Launch directory no longer exists"))
    (let ((buffer (agentd-new kind (plist-get state :directory) (plist-get state :arguments) name)))
      (with-current-buffer buffer
        (setq agentd-launch--pending-harness (plist-get state :harness))
        (add-hook 'agentd-buffer-update-hook #'agentd-launch--remember-harness nil t)
        (agentd-launch--remember-harness))
      buffer)))

;;;###autoload
(transient-define-prefix agentd-launch ()
  "Prepare a named agent in a project, worktree, or arbitrary directory."
  :refresh-suffixes t
  ["Agent session"
   ("a" (lambda () (agentd-launch--field "Harness" :harness)) agentd-launch-harness :transient t)
   ("n" (lambda () (agentd-launch--field "Name" :name)) agentd-launch-name :transient t)
   ("p" (lambda () (agentd-launch--field "Project" :project)) agentd-launch-project :transient t)
   ("w" (lambda () (agentd-launch--field "Worktree" :worktree)) agentd-launch-worktree :if agentd-launch--git-p :transient t)
   ("d" (lambda () (agentd-launch--directory-label)) agentd-launch-directory :transient t)]
  ["Options"
   ("c" "Create new worktree…" agentd-launch-create-worktree :if agentd-launch--git-p :transient t)
   ("-" (lambda () (format "Arguments  %s" (combine-and-quote-strings (plist-get agentd-launch--state :arguments)))) agentd-launch-arguments :transient t)]
  [("RET" (lambda () (agentd-launch--launch-label)) agentd-launch-start
    :inapt-if agentd-launch--problem)]
  (interactive)
  (setq agentd-launch--state (agentd-launch--initial-state default-directory))
  (transient-setup 'agentd-launch))

(provide 'agentd-launch)
;;; agentd-launch.el ends here
