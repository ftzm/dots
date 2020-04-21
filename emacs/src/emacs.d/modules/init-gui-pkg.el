;;; init-gui-el.el --- gui setup requiring external packages

(use-package highlight-parentheses
  :straight t
  :diminish highlight-parentheses-mode
  :config
  (add-hook 'prog-mode-hook 'highlight-parentheses-mode)
  (set-face-attribute 'hl-paren-face nil :bold t)
  )

(use-package rainbow-delimiters
  :straight t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  )

(use-package fringe-helper
  :straight t
  )

(use-package git-gutter
  :defer t
  :straight t
  :diminish git-gutter-mode
  )

(use-package git-gutter-fringe
  :straight t
  :after git-gutter
  :demand fring-helper
  :config
  ;;(load-theme 'gruvbox t)
  (set-face-attribute 'git-gutter:modified nil
		      :foreground 'unspecified
		      :background 'unspecified
		      :inherit 'font-lock-type-face)
  (set-face-attribute 'git-gutter:added nil
		      :foreground 'unspecified
		      :background 'unspecified
		      :inherit 'font-lock-string-face)
  (set-face-attribute 'git-gutter:deleted nil
		      :foreground 'unspecified
		      :background 'unspecified
		      :inherit 'font-lock-keyword-face)

  ;; subtle diff indicators in the fringe
  ;; places the git gutter outside the margins.
  (setq-default fringes-outside-margins t)
  ;; thin fringe bitmaps
  (fringe-helper-define 'git-gutter-fr:added '(center repeated)
    "XXX.....")
  (fringe-helper-define 'git-gutter-fr:modified '(center repeated)
    "XXX.....")
  (fringe-helper-define 'git-gutter-fr:deleted 'bottom
    "X......."
    "XX......"
    "XXX....."
    "XXXX....")

  )

(use-package default-text-scale
  :straight t
  :config
  (setq default-text-scale-amount 35)
  )

(use-package ace-window
  :straight t
  )

;(use-package rotate
;  :straight t
;  )

(use-package refresh-layout
  :straight (refresh-layout
	     :type git
	     :repo "refresh-layout-mode"
	     :branch "dev")
  :diminish refresh-layout-mode
  :config
  (refresh-layout-mode))





(provide 'init-gui-pkg)
