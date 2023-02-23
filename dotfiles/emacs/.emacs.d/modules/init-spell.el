(use-package flyspell
  :diminish "Sp"
  :commands flyspell-mode
  :hook ((org-mode) . flyspell-mode)
  :config
  (setq ispell-program-name (executable-find "aspell"))
  (add-hook 'text-mode-hook #'flyspell-mode)

  (setq ispell-local-dictionary-alist
      '((nil "[[:alpha:]]" "[^[:alpha:]]" "['’]" t nil nil utf-8)))
  )

(provide 'init-spell)
