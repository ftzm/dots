(use-package haskell-mode
  :straight t
  :mode "\\.hs\\'"
  :config
  (add-hook 'haskell-mode-hook 'auto-fill-mode)
  )

(use-package intero
  :straight t
  :diminish "\\"
  :config
  (add-hook 'haskell-mode-hook 'intero-mode)
  (with-eval-after-load 'flycheck
    (flycheck-add-next-checker 'intero '(warning . haskell-hlint)))
  )

(provide 'init-haskell)
