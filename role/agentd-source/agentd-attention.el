;;; agentd-attention.el --- Agent attention in the mode line -*- lexical-binding: t; -*-

(require 'agentd-status)

(defgroup agentd-attention nil "Attention for agent terminal buffers." :group 'applications)
(defface agentd-attention-icon
  '((t (:inherit error :family "Symbols Nerd Font Mono" :weight normal)))
  "Theme-colored Nerd Font icon for agents needing attention."
  :group 'agentd-attention)
(defface agentd-attention-elsewhere-icon
  '((t (:inherit warning :family "Symbols Nerd Font Mono" :weight normal)))
  "Theme-colored icon for attention outside the current workspace."
  :group 'agentd-attention)
(defcustom agentd-attention-delay 1
  "Seconds a fresh attention state must persist before it appears."
  :type 'number)
(defcustom agentd-attention-buffer-list-function #'buffer-list
  "Function returning the current workspace's buffers.
The picker uses this scope.  The indicator is red for attention here and
yellow for attention elsewhere; navigation visits this scope first.
An empty list means no buffers in the current workspace."
  :type 'function)
(defcustom agentd-attention-switch-buffer-function #'switch-to-buffer
  "Function displaying an attention buffer in its owning workspace.
Called with one buffer argument.  Workspace integrations should switch
workspace before displaying a buffer that belongs elsewhere."
  :type 'function)

(defvar-local agentd-attention-unseen-completion-p nil
  "Non-nil when this agent has completed work that has not been seen.
This is Emacs presentation state; the server's status remains `idle'.")
(defvar-local agentd-attention--previous nil)
(defvar-local agentd-attention--since 0)
(defvar-local agentd-attention--ready nil)
(defvar-local agentd-attention--timer nil)
(defvar agentd-attention-mode nil)

(defun agentd-attention--fresh-p ()
  (and (bound-and-true-p agentd-terminal-key)
       (eq agentd-connection-status 'connected)
       (not agentd-status-stale-p)))

(defun agentd-attention--visible-p ()
  "Whether this buffer is displayed in a focused, visible frame."
  (cl-some (lambda (window)
             (let ((frame (window-frame window)))
               (and (eq (frame-visible-p frame) t) (frame-focus-state frame))))
           (get-buffer-window-list (current-buffer) nil t)))

(defun agentd-attention--kind ()
  (pcase agentd-status
    ('waiting 'waiting)
    ('error 'error)
    ('idle (and agentd-attention-unseen-completion-p 'done))))

(defun agentd-attention--cancel ()
  (when agentd-attention--timer
    (cancel-timer agentd-attention--timer)
    (setq agentd-attention--timer nil)))

(defun agentd-attention--cleanup ()
  (agentd-attention--cancel)
  (setq agentd-attention--ready nil)
  (force-mode-line-update t))

(defun agentd-attention--settle (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq agentd-attention--timer nil)
      (when (and agentd-attention-mode (agentd-attention--fresh-p))
        (when (agentd-attention--visible-p)
          (setq agentd-attention-unseen-completion-p nil))
        (setq agentd-attention--ready (and (agentd-attention--kind) t)))
      (force-mode-line-update t))))

(defun agentd-attention--update ()
  "Observe the current buffer after a metadata update."
  (when (bound-and-true-p agentd-terminal-key)
    (add-hook 'kill-buffer-hook #'agentd-attention--cleanup nil t)
    (add-hook 'change-major-mode-hook #'agentd-attention--cleanup nil t)
    (if (not (agentd-attention--fresh-p))
        ;; Retain the last fresh status and unseen flag across disconnection.
        (progn (agentd-attention--cancel) (setq agentd-attention--ready nil))
      (let ((stamp (list agentd-status (plist-get agentd-session :status_since))))
        (unless (equal stamp agentd-attention--previous)
          (agentd-attention--cancel)
          (setq agentd-attention--ready nil
                agentd-attention-unseen-completion-p
                (and (eq agentd-status 'idle)
                     (or (and (eq (car agentd-attention--previous) 'idle)
                              agentd-attention-unseen-completion-p)
                         (memq (car agentd-attention--previous) '(working waiting))))
                agentd-attention--previous stamp
                agentd-attention--since
                (or (plist-get agentd-session :status_since) (* 1000 (float-time))))))
      (when (agentd-attention--visible-p)
        (setq agentd-attention-unseen-completion-p nil))
      (if (not (agentd-attention--kind))
          (progn (agentd-attention--cancel) (setq agentd-attention--ready nil))
        (unless (or agentd-attention--ready agentd-attention--timer)
          (setq agentd-attention--timer
                (run-at-time agentd-attention-delay nil
                             #'agentd-attention--settle (current-buffer))))))
    (force-mode-line-update t)))

(defun agentd-attention--observe-windows (&rest _)
  "Acknowledge completions displayed in focused frames."
  (when agentd-attention-mode
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and agentd-attention-unseen-completion-p
                   (agentd-attention--fresh-p) (agentd-attention--visible-p))
          (setq agentd-attention-unseen-completion-p nil
                agentd-attention--ready nil)
          (agentd-attention--cancel))))
    (force-mode-line-update t)))

(defun agentd-attention--priority (buffer)
  "Priority of BUFFER for both attention navigation and buffer completion."
  (with-current-buffer buffer
    (cond ((eq agentd-status 'exited) -2)
          ((not (agentd-attention--fresh-p)) -1)
          ((memq agentd-status '(waiting error)) 4)
          ((and (eq agentd-status 'idle) agentd-attention-unseen-completion-p) 3)
          ((eq agentd-status 'working) 2)
          ((eq agentd-status 'idle) 1)
          (t 0))))

(defun agentd-attention--sort (buffers)
  "Sort a copy of BUFFERS by priority, then newest transition, then name."
  (cl-stable-sort
   (copy-sequence buffers)
   (lambda (a b)
     (let ((a-priority (agentd-attention--priority a))
           (b-priority (agentd-attention--priority b))
           (a-since (or (plist-get (buffer-local-value 'agentd-session a) :status_since)
                        (buffer-local-value 'agentd-attention--since a)))
           (b-since (or (plist-get (buffer-local-value 'agentd-session b) :status_since)
                        (buffer-local-value 'agentd-attention--since b))))
       (cond ((/= a-priority b-priority) (> a-priority b-priority))
             ((/= a-since b-since) (> a-since b-since))
             (t (string-lessp (buffer-name a) (buffer-name b))))))))

(cl-defun agentd-attention-buffers
    (&optional (buffers (funcall agentd-attention-buffer-list-function)))
  "Return attention buffers in priority order, restricted to BUFFERS.
Waiting and error buffers come first, then unseen completions.  Within each
group, the most recent status transition comes first.  Explicit nil is empty."
  (let ((eligible
         (cl-remove-if-not
          (lambda (buffer)
            (and (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (and agentd-attention-mode agentd-attention--ready
                        (agentd-attention--fresh-p) (agentd-attention--kind)))))
          (delete-dups (copy-sequence buffers)))))
    (agentd-attention--sort eligible)))

;;;###autoload
(cl-defun agentd-next-attention
    (&optional (buffers nil supplied-p))
  "Visit the next agent needing attention, switching workspace if necessary.
Cycle through current-workspace agents first, then agents elsewhere, using
priority order within each group.  Explicit BUFFERS restricts navigation
to those buffers; explicit nil means empty.
Visiting a completion acknowledges it; waiting and error states remain
until the agent changes state."
  (interactive)
  (agentd-attention--observe-windows)
  (let* ((local (unless supplied-p (agentd-attention-buffers)))
         (candidates (if supplied-p
                         (agentd-attention-buffers buffers)
                       (append local (cl-remove-if
                                      (lambda (buffer) (memq buffer local))
                                      (agentd-attention-buffers (buffer-list))))))
         (tail (memq (current-buffer) candidates))
         (target (or (cadr tail) (car candidates))))
    (unless target (user-error "No agent buffers need attention"))
    (funcall agentd-attention-switch-buffer-function target)
    (agentd-attention--observe-windows)
    target))

(defvar agentd-attention--map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1]
                (lambda (event)
                  (interactive "e")
                  (select-window (posn-window (event-start event)))
                  (agentd-next-attention)))
    map))

(defun agentd-attention--mode-line ()
  (let* ((local (agentd-attention-buffers))
         (all (agentd-attention-buffers (buffer-list)))
         (elsewhere (cl-remove-if (lambda (buffer) (memq buffer local)) all)))
    (when all
      (propertize " \uee0d "
                  'face (if local 'agentd-attention-icon
                          'agentd-attention-elsewhere-icon)
                  'mouse-face 'mode-line-highlight
                  'help-echo (format "Agents needing attention: %d here, %d elsewhere; mouse-1: next agent"
                                     (length local) (length elsewhere))
                  'local-map agentd-attention--map))))

(defvar agentd-attention--mode-line-entry '(:eval (agentd-attention--mode-line)))

;;;###autoload
(define-minor-mode agentd-attention-mode
  "Show persistent agent attention in the mode line and track unseen work."
  :global t :group 'agentd-attention
  (if agentd-attention-mode
      (progn
        (unless (listp global-mode-string)
          (setq global-mode-string (list global-mode-string)))
        (add-to-list 'global-mode-string agentd-attention--mode-line-entry t)
        (add-hook 'agentd-buffer-update-hook #'agentd-attention--update)
        (add-hook 'post-command-hook #'agentd-attention--observe-windows)
        (add-hook 'window-buffer-change-functions #'agentd-attention--observe-windows)
        (add-function :after after-focus-change-function #'agentd-attention--observe-windows)
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer (agentd-attention--update))))
    (setq global-mode-string (delete agentd-attention--mode-line-entry global-mode-string))
    (remove-hook 'agentd-buffer-update-hook #'agentd-attention--update)
    (remove-hook 'post-command-hook #'agentd-attention--observe-windows)
    (remove-hook 'window-buffer-change-functions #'agentd-attention--observe-windows)
    (remove-function after-focus-change-function #'agentd-attention--observe-windows)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (bound-and-true-p agentd-terminal-key)
          (agentd-attention--cleanup)
          (setq agentd-attention--previous nil agentd-attention-unseen-completion-p nil)))))
  (force-mode-line-update t))

(provide 'agentd-attention)
;;; agentd-attention.el ends here
