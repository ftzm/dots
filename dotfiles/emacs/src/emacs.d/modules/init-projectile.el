(use-package projectile
  :straight t
  :diminish projectile-mode
  ;:after (ivy)
  :init
  (projectile-mode)
  :config
  ;(setq projectile-completion-system 'ivy) ;;requires ivy
  (setq projectile-completion-system 'default)

  ;; this improves speed by not being bogged down by zsh stuff
  (setq shell-file-name "/bin/sh")
  (setq projectile-enable-caching t)

  (setq projectile-globally-ignored-file-suffixes '("~" "#"))

  )

;(use-package counsel-projectile
;  :straight t
;  :after (projectile, counsel)
;  )

(provide 'init-projectile)
