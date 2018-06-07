(use-package dockerfile-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
  )

(provide 'init-dockerfile)
