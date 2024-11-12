(use-package elm-mode
  :straight t
  :mode ("\\.elm\\'" . elm-mode)
  :config
  (add-hook 'elm-mode-hook #'flycheck-mode)
  ;;(global-eldoc-mode -1) ;; probably disables it globally, will likely want a more subtle solution if I need it elsewhere
  ;; (setq-local eldoc-documentation-function #'ignore)  ;; <-- override eldoc function!
  )

(use-package flycheck-elm
  :straight t
  :after (elm-mode)
  :init
    (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-elm-setup))
  )

(provide 'init-elm)
