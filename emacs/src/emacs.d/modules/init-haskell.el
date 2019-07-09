(use-package haskell-mode
  :straight t
  :mode "\\.hs\\'"
  :config
  (add-hook 'haskell-mode-hook 'auto-fill-mode)


  )

(use-package dante
  :straight t
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'dante-mode-hook
  (add-hook 'haskell-mode-hook 'dante-mode)
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  ;; OR:
  ;; (add-hook 'haskell-mode-hook 'flymake-mode)
   '(lambda () (flycheck-add-next-checker 'haskell-dante
                '(warning . haskell-hlint))))

  ;; don't autosave with dante
  (setq flymake-no-changes-timeout nil)
  (setq flymake-start-syntax-check-on-newline nil)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  )
  :config
  (define-prefix-command 'haskell-mode-keys)
  (evil-define-key 'normal haskell-mode-map (kbd ",") 'haskell-mode-keys)
  (define-key haskell-mode-keys "c" 'flycheck-buffer)


;; (use-package intero
;;   :straight t
;;   :diminish "\\"
;;   :config
;;   (add-hook 'haskell-mode-hook 'intero-mode)
;;   (with-eval-after-load 'flycheck
;;     (flycheck-add-next-checker 'intero '(warning . haskell-hlint)))
;;   )


(provide 'init-haskell)
