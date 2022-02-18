(use-package magit
  :straight t
  :commands magit-status
  :custom (magit-bury-buffer-function #'magit-restore-window-configuration)
  :config
  (define-key transient-map        (kbd "<escape>") 'transient-quit-one)
  (define-key transient-edit-map   (kbd "<escape>") 'transient-quit-one)
  (define-key transient-sticky-map (kbd "<escape>") 'transient-quit-seq)
  )

(use-package autorevert
  :diminish auto-revert-mode
  )

(use-package git-link
  :commands git-link
  :straight t)

(provide 'init-magit)
