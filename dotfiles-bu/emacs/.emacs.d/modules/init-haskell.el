; workaround for emacs 30, check if necessary after package update
(setq flymake-allowed-file-name-masks nil)


(use-package haskell-mode
  :straight t
  :mode "\\.hs\\'"
  :hook ((haskell-mode . whitespace-mode))
  :config
  (add-hook 'haskell-mode-hook 'auto-fill-mode)

  (defun haskell-hydra-header ()
    "Haskell Hydra")

  (pretty-hydra-define haskell-hydra
    (:color blue
     :quit-key "q"
     :title (haskell-hydra-header))
    ("Commands" (( "f" ormolu-format-buffer "format buffer")
	         ( "e" (message "No good overall error report") "toggle error list")
	         ( "E" ftzm/flymake-diag-buffer "error at point")
	         ( "a" eglot-code-actions "code actions")
	         ( "r" xref-find-references "find references")
		 ( "n" (message "Haskell can't rename") "rename"))
     "LSP" (( "s" eglot "start lsp")
	    ( "i" lsp-metals-build-import "metals build+import")
	    ( "d" eldoc "describe thing at point"))))


  ;; Highlight line excess in red
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face lines-tail))



  (evil-define-key 'normal haskell-mode-map (kbd ",") 'haskell-hydra/body)

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



  ;; (use-package dante
  ;;   :straight t
  ;;   :after haskell-mode
  ;;   :commands dante-mode
  ;;   :hook ((haskell-mode . dante-mode)
  ;; 	 (haskell-mode . flycheck-mode))
  ;;   :config
  ;;   (add-hook 'haskell-mode-hook 'flycheck-mode)
  ;;   ;; OR:
  ;;   ;; (add-hook 'haskell-mode-hook 'flymake-mode)
  ;;    '(lambda () (flycheck-add-next-checker 'haskell-dante
  ;;                 '(warning . haskell-hlint)))

  ;;   ;; don't autosave with dante
  ;;   (setq flymake-no-changes-timeout nil)
  ;;   (setq flymake-start-syntax-check-on-newline nil)
  ;;   (setq flycheck-check-syntax-automatically '(save mode-enabled))

  ;;   (define-prefix-command 'haskell-mode-keys)
  ;;   (evil-define-key 'normal haskell-mode-map (kbd ",") 'haskell-mode-keys)
  ;;   (define-key haskell-mode-keys "c" 'flycheck-buffer)
  ;;   )

  (use-package whitespace
    :diminish))

(use-package ormolu
  :straight t
  :config
  (setq ormolu-process-path "fourmolu")
  )

(provide 'init-haskell)
