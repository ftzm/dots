(use-package company
  :straight t
  :diminish company-mode
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay nil)
  (define-key evil-insert-state-map "\C-n" 'company-complete)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (setq company-minimum-prefix-length 1)
  (setq company-require-match 'never) ;; allow breaking out by typing
  ;; Don't convert everything to lower case
  (setq company-dabbrev-downcase nil)
  ;(setq lsp-completion-provider :capf)
  )

(use-package company-box
  :straight t
  :diminish
  :hook (company-mode . company-box-mode))

(provide 'init-company)
