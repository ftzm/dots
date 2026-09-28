;;; agentd-perspective.el --- Perspective integration -*- lexical-binding: t; -*-

(require 'agentd-attention)

(declare-function persp-current-buffers* "perspective")
(declare-function persp-is-current-buffer "perspective")
(declare-function persp-buffer-in-other-p "perspective")
(declare-function persp-switch "perspective")

(defun agentd-perspective-buffers ()
  "Return current-perspective buffers, or all buffers when mode is off."
  (if (bound-and-true-p persp-mode)
      (persp-current-buffers*)
    (buffer-list)))

(defun agentd-perspective-switch-buffer (buffer)
  "Visit BUFFER in its existing perspective, including on another frame.
Shared buffers stay in the current perspective.  Unassigned buffers, or
buffers visited with Perspective mode off, use ordinary buffer switching."
  (when (and (bound-and-true-p persp-mode)
             (not (persp-is-current-buffer buffer)))
    (when-let* ((owner (persp-buffer-in-other-p buffer)))
      ;; Perspective's own buffer switcher only switches perspective when
      ;; the owning frame is already selected.  Select that frame first.
      (unless (eq (car owner) (selected-frame))
        (select-frame-set-input-focus (car owner)))
      (persp-switch (cdr owner))))
  (if-let* ((window (get-buffer-window buffer (selected-frame))))
      (select-window window)
    (switch-to-buffer buffer)))

(provide 'agentd-perspective)
;;; agentd-perspective.el ends here

(require 'agentd-recovery)
(declare-function persp-names "perspective")
(declare-function persp-get-buffers "perspective" (&optional perspective frame))
(declare-function persp-new "perspective" (name))
(declare-function persp-add-buffer "perspective" (buffer))

(defun agentd-perspective-memberships ()
  "Return all workspace names containing the current agent buffer."
  (when (bound-and-true-p persp-mode)
    (let ((buffer (current-buffer)) names)
      (dolist (frame (frame-list))
        (with-selected-frame frame
          (dolist (name (persp-names))
            (when (memq buffer (persp-get-buffers name)) (cl-pushnew name names :test #'equal)))))
      names)))

(defun agentd-perspective-restore (buffer names)
  "Restore BUFFER's memberships without displaying it or switching workspaces.
Missing workspaces are created on this frame. Names shared across frames restore
to an existing owner; frame identities do not survive an Emacs restart."
  (when (bound-and-true-p persp-mode)
    (dolist (name names)
      (let ((frame (or (cl-find-if (lambda (f) (with-selected-frame f (member name (persp-names))))
                                   (frame-list))
                       (selected-frame))))
        (with-selected-frame frame
          (let ((current (frame-parameter nil 'persp--curr))
                (perspective (save-window-excursion (persp-new name))))
            (unwind-protect
                (progn (set-frame-parameter nil 'persp--curr perspective)
                       (persp-add-buffer buffer))
              (set-frame-parameter nil 'persp--curr current))))))))

(setq agentd-recovery-memberships-function #'agentd-perspective-memberships
      agentd-recovery-restore-function #'agentd-perspective-restore)
(with-eval-after-load 'perspective
  (dolist (function '(persp-add-buffer persp-remove-buffer persp-forget-buffer persp-kill))
    (advice-add function :after #'agentd-recovery-schedule)))
