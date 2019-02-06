(use-package json-mode
  :mode (("\\.json\\'" . json-mode)
         ("\\.tmpl\\'" . json-mode)
         ("\\.eslintrc\\'" . json-mode))
  :config
  (add-hook 'json-mode-hook 'flycheck-mode)
  )

(provide 'init-json)
