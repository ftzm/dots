(use-package rust-mode
  :straight t
  :mode ".rs\\'"
  :interpreter
    ("rust" . rust-mode)
  :hook ((rust-mode . eglot-ensure))

  :config
  (add-to-list 'eglot-server-programs
               '(rust-mode . ("rust-analyzer")))
  )

(provide 'init-rust)
