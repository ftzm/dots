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

(use-package git-gutter
  :straight t
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode +1)
  )

(use-package git-gutter-fringe
  :straight t
  :config
  (require 'git-gutter) ;; needs to be loaded to set the below I think
  (load-theme 'gruvbox t)
  (set-face-attribute 'git-gutter:modified nil :foreground 'unspecified :inherit 'font-lock-type-face)
  (set-face-attribute 'git-gutter:added nil :foreground 'unspecified :inherit 'font-lock-string-face)
  (set-face-attribute 'git-gutter:deleted nil :foreground 'unspecified :inherit 'font-lock-keyword-face)

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
(provide 'init-gui-pkg)
