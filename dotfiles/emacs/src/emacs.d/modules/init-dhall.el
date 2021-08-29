(use-package dhall-mode
  :straight t
  :mode ("\\.dhall\\'" . dhall-mode)
  :config
  (setq dhall-use-header-line nil)
  )

(provide 'init-dhall)
