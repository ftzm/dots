(setq scroll-step 1
      scroll-conservatively 10000) ;; scroll by 1 like in vim

(use-package evil
  :straight t
  :init
  ;; for evil-collections
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-tree)
  ;; for visual line mode
  (setq evil-respect-visual-line-mode t)

(setq theme-bg (face-attribute 'default :background))
(setq theme-highlight (face-attribute 'highlight :background))
(setq theme-color-1 (face-attribute 'font-lock-function-name-face :foreground))
(setq theme-color-2 (face-attribute 'font-lock-string-face :foreground))
(setq evil-normal-state-tag   (propertize " N " 'face (list :background theme-color-1 :foreground theme-bg :weight 'bold))
      evil-emacs-state-tag    (propertize " E " 'face '((:background "orange" :foreground "black")))
      evil-insert-state-tag   (propertize " I " 'face (list :background theme-color-2 :foreground theme-bg :weight 'bold))
      evil-motion-state-tag   (propertize " M " 'face '((:background "blue") :foreground "white"))
      evil-visual-state-tag   (propertize " V " 'face '((:background theme-color-two :foreground "black")))
      evil-operator-state-tag (propertize " O " 'face '((:background "purple"))))
  :config
  (evil-mode t)





  (setq evil-insert-state-message nil)

  (evil-select-search-module 'evil-search-module 'evil-search)

  (define-key evil-normal-state-map (kbd "C-i") 'evil-jump-forward)
  ;(define-key evil-normal-state-map "L" 'evil-end-of-line)
  ;(define-key evil-visual-state-map "L" 'evil-last-non-blank)
  ;(define-key evil-normal-state-map "H" 'beginning-of-line-text)
  ;(define-key evil-visual-state-map "H" 'beginning-of-line-text)
  (define-key evil-normal-state-map (kbd "[ SPC") 'insert-line-above)
  (define-key evil-normal-state-map (kbd "] SPC") 'insert-line-below)
  (define-key evil-normal-state-map (kbd "[ d") 'delete-line-above)
  (define-key evil-normal-state-map (kbd "] d") 'delete-line-below)

  (evil-define-minor-mode-key 'normal 'visual-line-mode
    "^" 'evil-beginning-of-visual-line)

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
  (evil-collection-init 'vterm)
  (evil-collection-init 'emms)
  (evil-collection-init 'mu4e)
  )

(use-package evil-surround
  :straight t
  :config
  (global-evil-surround-mode 1)
  )

(provide 'init-evil)
