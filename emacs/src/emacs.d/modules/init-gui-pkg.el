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
  :diminish
  )

;(use-package rotate
;  :straight t
;  )

;(use-package refresh-layout
;  :straight (refresh-layout
;	     :type git
;	     :repo "refresh-layout-mode"
;	     :branch "dev")
;  :diminish refresh-layout-mode
;  :config
;  (refresh-layout-mode))


;(use-package composite
;  :ensure nil
;  :init
;  (defvar composition-ligature-table (make-char-table nil))
;  :hook
;  (((prog-mode conf-mode nxml-mode markdown-mode help-mode)
;    . (lambda () (setq-local composition-function-table composition-ligature-table))))
;  :config
;  ;; support ligatures, some toned down to prevent hang
;  (when (version<= "27.0" emacs-version)
;    (let ((alist
;           '((33 . ".\\(?:\\(==\\|[!=]\\)[!=]?\\)")
;             (35 . ".\\(?:\\(###?\\|_(\\|[(:=?[_{]\\)[#(:=?[_{]?\\)")
;             (36 . ".\\(?:\\(>\\)>?\\)")
;             (37 . ".\\(?:\\(%\\)%?\\)")
;             (38 . ".\\(?:\\(&\\)&?\\)")
;             (42 . ".\\(?:\\(\\*\\*\\|[*>]\\)[*>]?\\)")
;             ;; (42 . ".\\(?:\\(\\*\\*\\|[*/>]\\).?\\)")
;             (43 . ".\\(?:\\([>]\\)>?\\)")
;             ;; (43 . ".\\(?:\\(\\+\\+\\|[+>]\\).?\\)")
;             (45 . ".\\(?:\\(-[->]\\|<<\\|>>\\|[-<>|~]\\)[-<>|~]?\\)")
;             ;; (46 . ".\\(?:\\(\\.[.<]\\|[-.=]\\)[-.<=]?\\)")
;             (46 . ".\\(?:\\(\\.<\\|[-=]\\)[-<=]?\\)")
;             (47 . ".\\(?:\\(//\\|==\\|[=>]\\)[/=>]?\\)")
;             ;; (47 . ".\\(?:\\(//\\|==\\|[*/=>]\\).?\\)")
;             (48 . ".\\(?:\\(x[a-fA-F0-9]\\).?\\)")
;             (58 . ".\\(?:\\(::\\|[:<=>]\\)[:<=>]?\\)")
;             (59 . ".\\(?:\\(;\\);?\\)")
;             (60 . ".\\(?:\\(!--\\|\\$>\\|\\*>\\|\\+>\\|-[-<>|]\\|/>\\|<[-<=]\\|=[<>|]\\|==>?\\||>\\||||?\\|~[>~]\\|[$*+/:<=>|~-]\\)[$*+/:<=>|~-]?\\)")
;             (61 . ".\\(?:\\(!=\\|/=\\|:=\\|<<\\|=[=>]\\|>>\\|[=>]\\)[=<>]?\\)")
;             (62 . ".\\(?:\\(->\\|=>\\|>[-=>]\\|[-:=>]\\)[-:=>]?\\)")
;             (63 . ".\\(?:\\([.:=?]\\)[.:=?]?\\)")
;             (91 . ".\\(?:\\(|\\)[]|]?\\)")
;             ;; (92 . ".\\(?:\\([\\n]\\)[\\]?\\)")
;             (94 . ".\\(?:\\(=\\)=?\\)")
;             (95 . ".\\(?:\\(|_\\|[_]\\)_?\\)")
;             (119 . ".\\(?:\\(ww\\)w?\\)")
;             (123 . ".\\(?:\\(|\\)[|}]?\\)")
;             (124 . ".\\(?:\\(->\\|=>\\||[-=>]\\||||*>\\|[]=>|}-]\\).?\\)")
;             (126 . ".\\(?:\\(~>\\|[-=>@~]\\)[-=>@~]?\\)"))))
;      (dolist (char-regexp alist)
;        (set-char-table-range composition-ligature-table (car char-regexp)
;                              `([,(cdr char-regexp) 0 font-shape-gstring]))))
;    (set-char-table-parent composition-ligature-table composition-function-table))
;  )


(provide 'init-gui-pkg)
