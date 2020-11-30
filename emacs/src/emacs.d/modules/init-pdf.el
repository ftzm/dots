(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (evil-collection-init 'pdf)
  (evil-collection-pdf-setup)
  (evil-set-initial-state 'pdf-view-mode 'normal))


(provide 'init-pdf)
