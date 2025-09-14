;;; evil-tablist-visual-mark.el --- Mark tablist entries with Evil visual selection -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Generated with Claude Code
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.1") (evil "1.0") (tablist "1.0"))
;; Keywords: convenience, evil, tablist
;; URL: 

;;; Commentary:

;; This package provides automatic marking of tablist entries based on
;; Evil visual selection.  When you create a visual selection in
;; tablist-mode or tablist-minor-mode buffers, the selected lines are
;; automatically marked with the tablist marker character.  When you
;; exit visual mode, all marks are cleared.

;; The minor mode works by hooking into Evil's visual state entry and
;; exit events.  When visual mode is entered, it installs a
;; post-command hook to monitor selection changes.  As you move the
;; cursor to expand or contract the visual selection, the tablist marks
;; are updated in real-time to reflect the current selection.  Both
;; line-wise and character-wise visual selections are supported.

;; To use this minor mode, add the following to your configuration:

;;   (require 'evil-tablist-visual-mark)
;;   (evil-tablist-visual-mark-mode 1)

;; The mode automatically activates in any buffer with tablist-mode or
;; tablist-minor-mode enabled.

;;; Code:

(require 'evil)
(require 'tablist)
(require 'cl-lib)

(defvar-local evil-tablist--last-selected-lines nil
  "Last list of selected line numbers. Value is a list of integers or nil.")

(defvar-local evil-tablist--visual-marked-lines nil
  "List of line numbers marked during visual selection. Value is a list of integers or nil.")

(defvar-local evil-tablist--visual-start-line nil
  "Original line where visual selection started. Value is an integer or nil.")

(defun evil-tablist--update-visual-marking ()
  "Update tablist marking based on current visual selection. Return nothing."
  (when (evil-visual-state-p)
    (let ((current-lines (evil-tablist--get-selected-lines)))
      (unless (equal current-lines evil-tablist--last-selected-lines)
        (evil-tablist--sync-visual-marks current-lines evil-tablist--last-selected-lines)
        (setq evil-tablist--last-selected-lines current-lines)))))

(defun evil-tablist--get-visual-line-range ()
  "Get line range for visual line mode selection. Return a list of integers."
  (let* ((cursor-line (line-number-at-pos))
         (start-line (or evil-tablist--visual-start-line 
                        (setq evil-tablist--visual-start-line cursor-line))))
    (number-sequence (min start-line cursor-line) 
                    (max start-line cursor-line))))

(defun evil-tablist--get-selected-lines ()
  "Get list of currently selected line numbers. Return a list of integers or nil."
  (if (eq evil-this-type 'line)
      (evil-tablist--get-visual-line-range)
    (evil-tablist--region-to-lines)))

(defun evil-tablist--region-to-lines ()
  "Convert region to list of line numbers. Return a list of integers or nil."
  (when-let ((region (cons (marker-position evil-visual-beginning)
                           (marker-position evil-visual-end))))
    (let* ((beg-line (line-number-at-pos (car region)))
           (end-line (line-number-at-pos (cdr region)))
           (actual-end (save-excursion 
                         (goto-char (cdr region))
                         (if (bolp) (max beg-line (1- end-line)) end-line))))
      (when (<= beg-line actual-end)
        (number-sequence beg-line actual-end)))))

(defun evil-tablist--sync-visual-marks (new-lines old-lines)
  "Sync visual marks: mark new lines, unmark removed lines. Return nothing.

NEW-LINES is a list of integers or nil.
OLD-LINES is a list of integers or nil."
  (let ((to-mark (cl-set-difference new-lines (or old-lines '())))
        (to-unmark (cl-set-difference (or old-lines '()) new-lines)))
    (setq evil-tablist--visual-marked-lines 
          (cl-union (cl-set-difference evil-tablist--visual-marked-lines to-unmark) to-mark))
    (evil-tablist--apply-marks to-mark to-unmark)))

(defun evil-tablist--apply-marks (mark-lines unmark-lines)
  "Apply marking operations to tablist lines. Return nothing.

MARK-LINES is a list of integers or nil.
UNMARK-LINES is a list of integers or nil."
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t)
        (inhibit-point-motion-hooks t)
        (deactivate-mark nil))
    (with-silent-modifications
      (dolist (line mark-lines)
        (save-excursion
          (goto-line line)
          (when (tabulated-list-get-id)
            (tabulated-list-put-tag (string tablist-marker-char)))))
      (dolist (line unmark-lines)
        (save-excursion
          (goto-line line)
          (when (tabulated-list-get-id)
            (tabulated-list-put-tag " ")))))))

(defun evil-tablist--visual-enter ()
  "Set up visual selection marking."
  (setq evil-tablist--visual-start-line nil)
  (add-hook 'post-command-hook 'evil-tablist--update-visual-marking nil t))

(defun evil-tablist--visual-exit ()
  "Clean up visual selection marking."
  (remove-hook 'post-command-hook 'evil-tablist--update-visual-marking t)
  (when evil-tablist--visual-marked-lines
    (evil-tablist--apply-marks nil evil-tablist--visual-marked-lines))
  (setq evil-tablist--last-selected-lines nil
        evil-tablist--visual-marked-lines nil
        evil-tablist--visual-start-line nil))

(defun evil-tablist--setup-buffer ()
  "Set up visual marking hooks for current tablist buffer."
  (add-hook 'evil-visual-state-entry-hook 'evil-tablist--visual-enter nil t)
  (add-hook 'evil-visual-state-exit-hook 'evil-tablist--visual-exit nil t))

;;;###autoload
(define-minor-mode evil-tablist-visual-mark-mode
  "Minor mode for marking tablist entries with Evil visual selection."
  :global t
  :group 'evil-tablist
  (if evil-tablist-visual-mark-mode
      (progn
        (add-hook 'tablist-mode-hook 'evil-tablist--setup-buffer)
        (add-hook 'tablist-minor-mode-hook
                  (lambda ()
                    (when tablist-minor-mode
                      (evil-tablist--setup-buffer)))))
    (progn
      (remove-hook 'tablist-mode-hook 'evil-tablist--setup-buffer)
      (remove-hook 'tablist-minor-mode-hook
                   (lambda ()
                     (when tablist-minor-mode
                       (evil-tablist--setup-buffer)))))))

(provide 'evil-tablist-visual-mark)

;;; evil-tablist-visual-mark.el ends here
