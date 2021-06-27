(use-package yasnippet
  :straight t
  :commands yas-minor-mode
  :diminish yas-minor-mode
  )

(use-package yasnippet-snippets
  :straight t
  :after yasnippet
  )

(provide 'init-snippets)
