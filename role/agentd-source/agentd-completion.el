;;; agentd-completion.el --- Completion for agent buffers -*- lexical-binding: t; -*-

(require 'agentd-attention)
(require 'marginalia)

(defvar agentd-buffer-history nil "History for `agentd-switch-buffer'.")
(defvar agentd-completion--choices nil
  "Candidate-to-buffer mapping for the active agent picker.")

(cl-defun agentd-buffers
    (&optional (buffers (funcall agentd-attention-buffer-list-function)))
  "Return agent BUFFERS in the same priority order as attention navigation.
Include agents without attention: working, idle, unknown, stale, then exited.
Use the configured workspace scope by default; explicit nil means empty."
  (agentd-attention--sort
   (cl-remove-if-not
    (lambda (buffer)
      (and (buffer-live-p buffer)
           (with-current-buffer buffer (bound-and-true-p agentd-terminal-key))))
    (delete-dups (copy-sequence buffers)))))

(defun agentd-completion--text (text)
  "Keep TEXT on one annotation line without inherited text properties."
  (replace-regexp-in-string
   "[[:cntrl:]]" (lambda (match) (format "\\x%02x" (string-to-char match)))
   (substring-no-properties text) t t))

(defun agentd-completion-annotate (candidate)
  "Annotate agent buffer CANDIDATE with status, title and directory."
  (let ((buffer (or (cdr (assoc candidate agentd-completion--choices))
                    (get-buffer candidate))))
    (if (not (buffer-live-p buffer))
        (marginalia--fields ("(buffer closed)" :face 'shadow))
      (with-current-buffer buffer
        (let* ((fresh (agentd-attention--fresh-p))
               (status (if (and (eq agentd-status 'idle)
                                agentd-attention-unseen-completion-p)
                           "done (unseen)" (symbol-name agentd-status)))
               (status (if fresh status (concat status " (stale)")))
               (title (agentd-completion--text (or agentd-title "—")))
               (directory (agentd-completion--text (abbreviate-file-name default-directory))))
          (marginalia--fields
           (status :width 21
                   :face (cond ((not fresh) 'shadow)
                               ((eq agentd-status 'error) 'error)
                               ((eq agentd-status 'waiting) 'warning)
                               (t 'success)))
           (title :truncate 0.4 :face 'marginalia-documentation)
           (directory :truncate -0.6 :face 'marginalia-file-name)))))))

(defun agentd-completion--table (choices)
  "Make an ordered completion table for buffer-name/BUFFER alist CHOICES."
  (lambda (string predicate action)
    (if (eq action 'metadata)
        '(metadata
          (category . agentd-buffer)
          (display-sort-function . identity)
          (cycle-sort-function . identity))
      (complete-with-action action choices string predicate))))

;;;###autoload
(cl-defun agentd-switch-buffer
    (&optional (buffers (funcall agentd-attention-buffer-list-function)))
  "Switch to an agent buffer using annotated, priority-ordered completion.
Use the configured workspace scope, or explicit BUFFERS.
Visit the selected buffer in its owning workspace.
Includes detached, stale and exited agent buffers.  Never creates a buffer."
  (interactive)
  (let* ((choices (mapcar (lambda (buffer) (cons (buffer-name buffer) buffer))
                          (agentd-buffers buffers)))
         (agentd-completion--choices choices)
         ;; Read status again on redisplay instead of caching it for the picker.
         (marginalia--cache-size 0))
    (unless choices (user-error "No agent buffers in scope"))
    (let* ((choice (completing-read "Agent buffer: " (agentd-completion--table choices)
                                    nil t nil 'agentd-buffer-history (caar choices)))
           (buffer (cdr (assoc choice choices))))
      ;; Resolve against buffer objects, so a rename during completion is safe.
      (unless (and (buffer-live-p buffer)
                   (with-current-buffer buffer (bound-and-true-p agentd-terminal-key)))
        (user-error "That agent buffer is no longer available"))
      (funcall agentd-attention-switch-buffer-function buffer)
      (agentd-attention--observe-windows)
      buffer)))

;;;###autoload
(defun agentd-switch-buffer-all ()
  "Select an agent buffer across all workspaces and visit its workspace."
  (interactive)
  (agentd-switch-buffer (buffer-list)))

;; Replace the old builtin-only registration when reloading an installed copy.
(setf (alist-get 'agentd-buffer marginalia-annotators)
      '(agentd-completion-annotate none))

(provide 'agentd-completion)
;;; agentd-completion.el ends here
