(use-package yaml-mode
  :straight t
  :defer t
  :mode (("\\.yaml\\'" . yaml-mode)
	 ("\\.yml\\'" . yaml-mode))
  )

(provide 'init-yaml)
