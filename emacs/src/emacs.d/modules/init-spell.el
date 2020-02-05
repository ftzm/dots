(use-package flyspell
  :diminish "Sp"
  :config
  (setq ispell-program-name (executable-find "aspell"))
  (add-hook 'text-mode-hook #'flyspell-mode)
  )

(use-package flyspell-correct-ivy
  :straight t
  :bind ("C-M-;" . flyspell-correct-wrapper)
  :init
  (setq flyspell-correct-interface #'flyspell-correct-ivy))

(provide 'init-spell)
