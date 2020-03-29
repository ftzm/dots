(use-package go-mode
  :straight t
  :mode ("\\.go\\'" . go-mode)
  :config
  (add-hook 'go-mode-hook 'flycheck-mode)
  (add-hook 'before-save-hook #'gofmt-before-save))

(provide 'init-go)
