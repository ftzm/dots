(use-package flyspell
  :diminish "Sp"
  :commands flyspell-mode
  :hook ((org-mode) . flyspell-mode)
  :config
  (setq ispell-program-name (executable-find "aspell"))
  (add-hook 'text-mode-hook #'flyspell-mode)
  )

(provide 'init-spell)
