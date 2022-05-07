(use-package vterm
  ;:commands vterm
  :config

  (add-hook 'vterm-mode-hook (lambda ()
  			       (setq-local global-hl-line-mode nil)))

  ;; ;The upstream of this keeps flaking out on me, defining it here for security
  ;; (defun ftzm/vterm-send-return ()
  ;;   "Sends C-m to the libvterm."
  ;;   (interactive)
  ;;   (process-send-string vterm--process "\C-m"))

  ;; (define-key vterm-mode-map [return]                    #'ftzm/vterm-send-return)

  (setq vterm-shell (getenv "SHELL"))


  (defun ftzm/bash-history ()
  (interactive)
  (let ((lines (delete-dups (reverse (with-temp-buffer
  				       (insert-file-contents "~/.bash_history")
  				       (split-string (buffer-string) "\n" t))))))
    (vterm-send-string (completing-read "Command: " lines))))

  (evil-define-key '(normal insert) vterm-mode-map
    (kbd "C-r") 'ftzm/bash-history)

  ; Makes vterm respond to input faster and feel more snappy.
  ; but fucks up evil apparently.
  ;(setq vterm-timer-delay 0.01)


  )

;(use-package vterm-toggle
;  :straight t)
;
;(use-package multi-libvterm
;  :straight (multi-libvterm
;	     :type git
;	     :host github
;	     :repo "suonlight/multi-libvterm"))

(provide 'init-vterm)
