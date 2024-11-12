(use-package pdf-tools
  :ensure t
  :defer t
  :config
  (pdf-tools-install)
  (evil-collection-init 'pdf)
  (evil-collection-pdf-setup)
  (evil-set-initial-state 'pdf-view-mode 'normal))

(use-package saveplace
  :hook ((pdf-view-mode . save-place-local-mode))
  :config
  (setq save-place-file (locate-user-emacs-file "places"))
  (require 'saveplace-pdf-view))

(use-package saveplace-pdf-view
  :after pdf-tools
  :straight t
  )

(provide 'init-pdf)
