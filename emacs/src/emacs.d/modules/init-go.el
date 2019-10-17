(use-package go-mode
  :straight t
  :config
  (add-hook 'go-mode-hook 'flycheck-mode)
  (add-hook 'before-save-hook #'gofmt-before-save))

(provide 'init-go)
