(use-package direnv
  :after lsp-mode
  :straight t
  :config
  (advice-add 'lsp :before #'direnv-update-environment)
  (direnv-mode)
  )

(provide 'init-direnv)
