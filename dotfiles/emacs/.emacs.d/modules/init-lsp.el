(use-package eglot
  :straight t
  :hook ((haskell-mode . eglot-ensure))
  :config
  )

(provide 'init-lsp)
