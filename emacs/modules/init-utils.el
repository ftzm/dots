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

(provide 'init-utils)
