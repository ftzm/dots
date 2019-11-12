(use-package flyspell
  :diminish "Sp"
  :config
  (setq ispell-program-name (executable-find "hunspell")
        ispell-dictionary "en_US")
  )

(use-package flyspell-correct-ivy
  :straight t
  :bind ("C-M-;" . flyspell-correct-wrapper)
  :init
  (setq flyspell-correct-interface #'flyspell-correct-ivy))

(provide 'init-spell)
