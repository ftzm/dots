(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

;; from doom
(defun sudo-find-file (file)
  "Open a file as root."
  (interactive
   (list (read-file-name "Open as root: ")))
  (find-file (if (file-writable-p file)
                 file
               (concat "/sudo:root@localhost:" file))))

;; from doom
(defun sudo-this-file ()
  "Open the current file as root."
  (interactive)
  (sudo-find-file (file-truename buffer-file-name)))

(defun ftzm/revert-all-file-buffers ()
  "Refresh all open file buffers without confirmation.
Buffers in modified (not yet saved) state in emacs will not be reverted. They
will be reverted though if they were modified outside emacs.
Buffers visiting files which do not exist any more or are no longer readable
will be killed."
  (interactive)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      ;; Revert only buffers containing files, which are not modified;
      ;; do not try to revert non-file buffers like *Messages*.
      (when (and filename
                 (not (buffer-modified-p buf)))
        (if (file-readable-p filename)
            ;; If the file exists and is readable, revert the buffer.
            (with-current-buffer buf
              (revert-buffer :ignore-auto :noconfirm :preserve-modes))
          ;; Otherwise, kill the buffer.
          (let (kill-buffer-query-functions) ; No query done when killing buffer
            (kill-buffer buf)
            (message "Killed non-existing/unreadable file buffer: %s" filename))))))
  (message "Finished reverting buffers containing unmodified files."))

(defun ftzm/flymake-diag-buffer ()
  (interactive)
  (let* ((current-point (point))
	 (errors-at-point (seq-filter
			   (lambda (diag)
			     (and (<= (flymake-diagnostic-beg diag)
				      current-point)
				  (<= current-point
				      (flymake-diagnostic-end diag))))
			   (flymake-diagnostics))))
    (if errors-at-point
	(with-help-window "*error-at-point*"
	  (mapc (lambda (d)
		  (princ (flymake-diagnostic-text d))
		  (princ "\n\n"))
		errors-at-point))
      (message "No flymake error at point."))))

(provide 'init-utils)
