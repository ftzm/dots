(use-package purescript-mode
  :straight t
  :mode "\\.purs$")

(use-package psc-ide
  :straight t
  :config
  (add-hook 'purescript-mode-hook
    (lambda ()
      (psc-ide-mode)
      (company-mode)
      (flycheck-mode)
      (turn-on-purescript-indentation))))

(provide 'init-purescript)
