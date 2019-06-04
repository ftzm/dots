(use-package evil
  :straight t
  :init
  ;; for evil-collections
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode t)

  (evil-select-search-module 'evil-search-module 'evil-search)

  (define-key evil-normal-state-map (kbd "C-i") 'evil-jump-forward)
  (define-key evil-normal-state-map "L" 'evil-end-of-line)
  (define-key evil-visual-state-map "L" 'evil-last-non-blank)
  (define-key evil-normal-state-map "H" 'beginning-of-line-text)
  (define-key evil-visual-state-map "H" 'beginning-of-line-text)
  (define-key evil-normal-state-map (kbd "[ SPC") 'add-line-above)
  (define-key evil-normal-state-map (kbd "] SPC") 'add-line-below)

  )

(use-package evil-collection
  :straight t
  :config
  ;; add more as need by this pattern
  (evil-collection-init 'dired)
  )

(use-package evil-surround
  :straight t
  :config
  (global-evil-surround-mode 1)
  )

(provide 'init-evil)
