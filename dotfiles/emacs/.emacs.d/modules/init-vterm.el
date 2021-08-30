(use-package vterm
  :config

  (add-hook 'vterm-mode-hook (lambda ()
			       (setq-local global-hl-line-mode nil)))

  ;The upstream of this keeps flaking out on me, defining it here for security
  (defun ftzm/vterm-send-return ()
    "Sends C-m to the libvterm."
    (interactive)
    (process-send-string vterm--process "\C-m"))
  (define-key vterm-mode-map [return]                    #'ftzm/vterm-send-return)

  (setq vterm-shell (getenv "SHELL"))

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
