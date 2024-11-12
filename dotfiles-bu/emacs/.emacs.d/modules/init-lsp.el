(use-package eglot
  :straight t
  :hook ((haskell-mode . eglot-ensure))
  :config
  ; disable snippets in completion
  (fset #'eglot--snippet-expansion-fn #'ignore)
  )

(provide 'init-lsp)
