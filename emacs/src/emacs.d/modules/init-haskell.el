(use-package haskell-mode
  :straight t
  :mode "\\.hs\\'"
  :config
  (add-hook 'haskell-mode-hook 'auto-fill-mode)

  ;(defun haskell-evil-open-above ()
  ;  (interactive)
  ;  (evil-digit-argument-or-evil-beginning-of-line)
  ;  (haskell-indentation-newline-and-indent)
  ;  (evil-previous-line)
  ;  (haskell-indentation-indent-line)
  ;  (evil-append-line nil))

  ;(defun haskell-evil-open-below ()
  ;  (interactive)
  ;  (evil-append-line nil)
  ;  (haskell-indentation-newline-and-indent))

  ;  (evil-define-key 'normal haskell-mode-map "o" 'haskell-evil-open-below
  ;                                            "O" 'haskell-evil-open-above)

  )

(use-package dante
  :straight t
  :after haskell-mode
  :commands 'dante-mode
  :hook ((haskell-mode . dante-mode)
	 (haskell-mode . flycheck-mode))
  :config
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  ;; OR:
  ;; (add-hook 'haskell-mode-hook 'flymake-mode)
   '(lambda () (flycheck-add-next-checker 'haskell-dante
                '(warning . haskell-hlint)))

  ;; don't autosave with dante
  (setq flymake-no-changes-timeout nil)
  (setq flymake-start-syntax-check-on-newline nil)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))

  (define-prefix-command 'haskell-mode-keys)
  (evil-define-key 'normal haskell-mode-map (kbd ",") 'haskell-mode-keys)
  (define-key haskell-mode-keys "c" 'flycheck-buffer)
  )


;; (use-package intero
;;   :straight t
;;   :diminish "\\"
;;   :config
;;   (add-hook 'haskell-mode-hook 'intero-mode)
;;   (with-eval-after-load 'flycheck
;;     (flycheck-add-next-checker 'intero '(warning . haskell-hlint)))
;;   )


(provide 'init-haskell)
