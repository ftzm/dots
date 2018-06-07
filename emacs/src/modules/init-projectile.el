(use-package projectile
  :straight t
  :diminish projectile-mode
  :after (ivy)
  :init
  (projectile-mode)
  :config
  (setq projectile-completion-system 'ivy) ;;requires ivy
  )

(use-package counsel-projectile
  :straight t
  :after (projectile, counsel)
  )

(provide 'init-projectile)
