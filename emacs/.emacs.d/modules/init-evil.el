(use-package evil
  :straight t
  :init
  (evil-mode t)
  :config

  (evil-select-search-module 'evil-search-module 'evil-search)

  (define-key evil-normal-state-map (kbd "C-i") 'evil-jump-forward)
  (define-key evil-normal-state-map "L" 'evil-end-of-line)
  (define-key evil-visual-state-map "L" 'evil-last-non-blank)
  (define-key evil-normal-state-map "H" 'beginning-of-line-text)
  (define-key evil-visual-state-map "H" 'beginning-of-line-text)
  (define-key evil-normal-state-map (kbd "[ SPC") 'add-line-above)
  (define-key evil-normal-state-map (kbd "] SPC") 'add-line-below)

  )

(use-package evil-surround
  :straight t
  :config
  (global-evil-surround-mode 1)
  )

(provide 'init-evil)
