(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :hook ((scala-mode . copilot-mode))
  :config)

(provide 'init-copilot)
