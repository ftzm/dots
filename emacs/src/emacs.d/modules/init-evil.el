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
  (define-key evil-normal-state-map (kbd "[ SPC") 'insert-line-above)
  (define-key evil-normal-state-map (kbd "] SPC") 'insert-line-below)
  (define-key evil-normal-state-map (kbd "[ d") 'delete-line-above)
  (define-key evil-normal-state-map (kbd "] d") 'delete-line-below)

  (defun insert-line-below ()
  "Insert an empty line below the current line."
  (interactive)
  (save-excursion
    (end-of-line)
    (open-line 1))
  (undo-boundary)
  )

  (defun insert-line-above ()
  "Insert an empty line above the current line."
  (interactive)
  (save-excursion
    (end-of-line 0)
    (open-line 1))
  (undo-boundary))
  )

  (defun delete-line-above ()
  "Delete line below the current line."
  (interactive)
  (save-excursion
    (previous-line)
    (kill-whole-line))
  (undo-boundary)
  )

  (defun delete-line-below ()
  "Delete line below the current line."
  (interactive)
  (save-excursion
    (next-line)
    (kill-whole-line))
  (undo-boundary)
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
