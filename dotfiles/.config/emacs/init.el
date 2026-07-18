;; -*- lexical-binding: t; -*-

;; ----------------------------------------------------------------------------
;; Elpaca

;; Suppress elpaca log display in daemon mode
(add-hook 'elpaca-log-functions (lambda (&rest _) (and (daemonp) 'silent)))

;; silence error about this begin unset
(setq elpaca-core-date '(20240101))

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-current-buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; post recipe setup
(setq package-enable-at-startup nil)
;; Block until current queue processed.
(elpaca-wait)

(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq use-package-always-ensure t)
  )

;; ----------------------------------------------------------------------------

(set-frame-font
 (format "Iosevka ftzm Medium %d"
  (pcase (system-name)
    ("ftzm-P14s" 12)
    ("saoiste" 18)
    ("eachtrai" 18))))
					;(set-frame-font "Jetbrains Mono Medium 17")

;; automatically balances windows when  splitting.
(setq window-combination-resize t)

;; don't highlight bookmarks orange
(setq bookmark-fontify nil)

;; no blink
(blink-cursor-mode 0)
;;; no blink in term
(setq visible-cursor nil)
;;; highlint current line
(global-hl-line-mode)

(defun eval-after-load-all (my-features form)
  "Run FORM after all MY-FEATURES are loaded.
See `eval-after-load' for the possible formats of FORM."
  (if (null my-features)
      (if (functionp form)
	  (funcall form)
	(eval form))
    (eval-after-load (car my-features)
      `(lambda ()
	 (eval-after-load-all
	  (quote ,(cdr my-features))
	  (quote ,form))))))

(fringe-mode '(0 . 0))

(use-package jetbrains-darcula-theme
  :config
  )

(use-package gruvbox-theme
  :config
					;(load-theme 'gruvbox-dark-medium t)
  )

;; ----------------------------------------------------------------------------

(use-package emacs
  :ensure nil
  :config
  ;; disable electric indent mode by default (too many annoying conflicts with other formatters)
  (electric-indent-mode -1)
  ;; a more nuclear option to disable it if necessary, might make it hard to selectively enable though
					; (add-hook 'after-change-major-mode-hook (lambda() (electric-indent-mode -1)))
  ;; enable visual line mode everywhere
  (global-visual-line-mode 1)

  (setq use-short-answers t)            ;; y/n instead of typing out yes/no
  (setq help-window-select t)           ;; auto-focus help buffer when opened (q to dismiss)
  (setq require-final-newline t)        ;; ensure files end with newline (POSIX, avoids git noise)

  ;; Smooth pixel-level scrolling instead of line-by-line jumps (Emacs 29+)
  (pixel-scroll-precision-mode 1)

  ;; Track recently opened files for quick access via consult-recent-file
  (recentf-mode 1)
  (setq recentf-max-saved-items 200)

  ;; Max semantic highlighting for tree-sitter modes (built-in, default is 3).
  ;; Level 4 distinguishes function calls from definitions, highlights
  ;; escape sequences in strings, bracket highlighting, etc.
  (setq treesit-font-lock-level 4)
  )

;; ----------------------------------------------------------------------------

(use-package doom-themes
  :config
  (load-theme 'doom-gruvbox t)
  ;; doom-themes italicizes function calls; turn that off.
  (set-face-attribute 'font-lock-function-call-face nil :slant 'normal)
  ;; Use the default foreground for function defs and calls (was green).
  (let ((fg (face-attribute 'default :foreground)))
    (set-face-attribute 'font-lock-function-name-face nil :foreground fg)
    (set-face-attribute 'font-lock-function-call-face nil :foreground fg))
  ;; Dim the window divider to mode-line-inactive's grey. Source the color
  ;; from the palette (doom-color), not (face-foreground 'mode-line-inactive):
  ;; the latter returns nil at daemon-init and leaves the border the light
  ;; default fg until the first eval-buffer.
  (set-face-background 'vertical-border nil)
  (set-face-foreground 'vertical-border (doom-color 'base4))
  ;; Must be used *after* the theme is loaded
  ;; (custom-set-faces
  ;;  `(font-lock-type-face ((t (:foreground ,(doom-color 'violet)))))
  ;;  `(font-lock-doc-face ((t (:foreground ,(doom-color 'base6)))))
  ;;  `(font-lock-function-name-face ((t (:foreground ,(doom-color 'yellow)))))
  ;;  `(font-lock-preprocessor-face ((t (:foreground ,(doom-color 'orange))))))
  )

;; stop emacs from scattering temporary and app files everywhere
(use-package no-littering
  :demand t
  :config
  (no-littering-theme-backups)
  (let ((dir (no-littering-expand-var-file-name "lock-files/")))
    (make-directory dir t)
    (setq lock-file-name-transforms `((".*" ,dir t))))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))

;; Modern icon set for Emacs (replacement for all-the-icons).
;; Powers icons in doom-modeline, corfu, etc.
(use-package nerd-icons)

;; Auto-applies project formatting rules from .editorconfig files:
;; indent style (tabs/spaces), indent size, charset, trailing whitespace, final newline.
;; Most open-source projects include one. Zero manual config needed.
(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package undo-fu)

;; Kitty Keyboard Protocol support so terminals can transmit key combos
;; that ASCII can't encode (C-RET, C-., C-,, S-RET, etc.). Negotiates with
;; foot/kitty/wezterm/etc. Works through tmux when tmux has
;; `extended-keys on` and `terminal-features ',*:extkeys'` set.
;; Harmless in GUI Emacs (no-op).
(use-package kkp
  :config
  (global-kkp-mode +1))

;; Camel Case recognition, works with evil mode movement
(use-package subword
  :diminish subword-mode
  :ensure nil
  :config
  (add-hook 'prog-mode-hook 'subword-mode)
  )

(use-package evil
  :demand t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-fu)
  :config
  
  (evil-mode 1)
  ;; this makes these commands not count as "edits" so that I can jump
  ;; to the next error without overwriting the last edit in
  ;; `evil-repeat`
  (evil-declare-motion 'flymake-goto-next-error)
  (evil-declare-motion 'flymake-goto-prev-error)
  (evil-define-key 'normal 'global (kbd "C-u") 'evil-scroll-up)

					;(when (not (display-graphic-p))
					;(setq evil-insert-state-cursor '(box "red")))

  )

(use-package general
  :demand t
  :init
  (general-evil-setup)
  
  ;;; Vim Mappings

  (setq general-override-states '(insert
                                  emacs
                                  hybrid
                                  normal
                                  visual
                                  motion
                                  operator
                                  replace))

  (general-nmap "c" ;; this must be defined after evil to bind c
    (general-key-dispatch 'evil-change
      "c" (general-simulate-key ('evil-change "e"))
      ))
  (general-vmap "c" 'evil-change)
  (general-nmap "s" 'flash-jump)


  (general-create-definer my-leader-def
    :prefix "SPC")

  (my-leader-def
    :states '(normal visual motion)
    :keymaps 'override
    "SPC" '(execute-extended-command :which-key "command")
    ;; the default behavior of `other buffer' is to ignore buffers
    ;; Toggle to the last buffer visited in this window.
    ;; Per-window tracking avoids confusion with split layouts.
    "'" '(evil-switch-to-windows-last-buffer :which-key "last buffer")
    "," '(ftzm/flip-window :which-key "previous window")
    "d" '(dired-jump :which-key "dired here")
    "D" '(dired :which-key "dired")
					;"a" '(app-keys :which-key "apps")
    "b" '(buffer-keys :which-key "buffer")
					;"e" '(flycheck-keys :which-key "error")
					;"i" '(ivy-keys :which-key "ivy")
    "g" '(magit-keys :which-key "git")
    "o" '(org-global-hydra/body :which-key "org")
    "t" '(term-hydra/body :which-key "terminal")
    "p" '(project-keys :which-key "project")
    "w" '(persp-hydra/body :which-key "workspace")
					;"t" '(toggle-keys :which-key "toggle")
					;"w" '(window-keys :which-key "window")
    "/" '(consult-ripgrep :which-key "search project")
    "." '(vertico-repeat :which-key "repeat search")
    "s" '(search-keys :which-key "search")
    "m" '(mpd-hydra/body :which-key "mpd")
					;"M" '(mail-hydra/body :which-key "mail")
    "0" 'winum-select-window-0
    "1" 'winum-select-window-1
    "2" 'winum-select-window-2
    "3" 'winum-select-window-3
    "4" 'winum-select-window-4
    "5" 'winum-select-window-5
    "6" 'winum-select-window-6
    "7" 'winum-select-window-7
    "8" 'winum-select-window-8
    "9" 'winum-select-window-9
    )

  (general-define-key
   :prefix-command 'buffer-keys
   "d" 'kill-current-buffer
   "e" 'eval-buffer
					;"k" 'evil-prev-buffer
					;"j" 'evil-next-buffer
   "b" 'consult-buffer
   "B" 'consult-buffer-other-window
   "s" 'save-buffer
   "f" 'find-file
   "w" 'write-file
   "r" 'consult-recent-file
   "RET" 'consult-bookmark  ;; TODO: scope by project via custom consult source
                            ;; that filters to current perspective's project root
   "m" 'bookmark-set
   "D" 'kill-buffer-and-window-if-split
					;"u" 'sudo-find-file
					;"U" 'sudo-this-file
   )

  (defun kill-buffer-and-window-if-split ()
    "Kill current buffer. Also delete the window if there are multiple."
    (interactive)
    (let ((buf (current-buffer)))
      (if (> (count-windows) 1)
          (progn (kill-buffer buf) (delete-window))
        (kill-buffer buf))))

  (general-define-key
   :prefix-command 'search-keys
   "s" 'consult-eglot-symbols
   "o" 'consult-outline
   "m" 'consult-mark)

  (general-define-key
   :keymaps '(override vertico-map)
   "M-0" 'winum-select-window-0
   "M-1" 'winum-select-window-1
   "M-2" 'winum-select-window-2
   "M-3" 'winum-select-window-3
   "M-4" 'winum-select-window-4
   "M-5" 'winum-select-window-5
   "M-6" 'winum-select-window-6
   "M-7" 'winum-select-window-7
   "M-8" 'winum-select-window-8
   "M-9" 'winum-select-window-9
   )


  (general-define-key
   :prefix-command 'window-keys
   "v" 'split-window-right
   "s" 'split-window-below
   "h" 'evil-window-left
   "j" 'evil-window-down
   "k" 'evil-window-up
   "l" 'evil-window-right
   "H" 'evil-window-move-far-left
   "J" 'evil-window-move-very-bottom
   "K" 'evil-window-move-very-top
   "L" 'evil-window-move-far-right
   "R" 'evil-window-rotate-upwards
   "d" 'delete-window
   "w" 'evil-window-next)

  (general-define-key
   :states '(normal insert)
   :keymaps 'override
   "C-n" 'completion-at-point)
  )



;; necessary to wait for the :general keyword
(elpaca-wait)


(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; gc operator for commenting (like vim-commentary)
;; gcc = comment/uncomment line, gc in visual = comment selection,
;; gcap = comment paragraph, gc3j = comment 3 lines down
(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

;; Shows register/mark contents in a popup when you press " (registers)
;; or '/ ` (marks). No more guessing what's stored where.
(use-package evil-owl
  :after evil
  :config
  (setq evil-owl-max-string-length 500)  ;; truncate long register contents
  (add-to-list 'display-buffer-alist
               '("*evil-owl*"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.3)))
  (evil-owl-mode))

;; Automatic tablist marking on visual selection
(use-package tablist)

(use-package evil-tablist-visual-mark
  :ensure nil
  :load-path "lisp/"
  :after (evil tablist)
  :config
  (evil-tablist-visual-mark-mode 1))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (evil-define-key 'normal org-mode-map (kbd "TAB") 'org-cycle))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode)
  (setq which-key-idle-delay 0.3)
  ;; Show transient keymaps (e.g. consult narrow map after pressing <).
  (setq which-key-show-transient-maps t)

  ;; Use near-zero which-key delay in the minibuffer so consult
  ;; narrow options appear instantly after pressing <.
  (defvar my/which-key-default-delay 0.3)
  (add-hook 'minibuffer-setup-hook
            (lambda ()
              (setq my/which-key-default-delay which-key-idle-delay
                    which-key-idle-delay 0.01)
              (which-key--start-timer)))
  (add-hook 'minibuffer-exit-hook
            (lambda ()
              (setq which-key-idle-delay my/which-key-default-delay)
              (which-key--start-timer))))


(defun ftzm/flip-window ()
  (interactive)
  (let ((win  (get-mru-window nil nil t)))
    (when win (select-window win))))

(use-package winum
  :init
  :config
  (setq window-numbering-scope            'frame
        winum-reverse-frame-list          nil
        winum-auto-assign-0-to-minibuffer t
					;winum-assign-func                 'my-winum-assign-func
					;winum-auto-setup-mode-line        t
        winum-format                      nil
        winum-mode-line-position          nil
					;winum-ignored-buffers             '(" *which-key*")
					;winum-ignored-buffers-regexp      '(" \\*Treemacs-.*")
        winum-ignored-buffers-regexp      '()
	)
  (winum-mode)
  )

(use-package doom-modeline
  :config
  (doom-modeline-def-modeline 'trimmed
    '(eldoc bar workspace-name matches follow buffer-info remote-host buffer-position word-count parrot selection-info)
    '(compilation objed-state misc-info project-name persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process check time))

  ;; Set default mode-line
  (add-hook 'doom-modeline-mode-hook
            (lambda ()
              (doom-modeline-set-modeline 'trimmed 'default)))

  (doom-modeline-mode 1)

  ;; must be set after mode enabled apparently
  (add-to-list
   'mode-line-misc-info
   `(eglot--managed-mode (" [" eglot--mode-line-format "] "))
   )
  (setq doom-modeline-lsp nil)

  (setq doom-modeline-percent-position '(-3 ""))
  (setq column-number-mode t
	line-number-mode t)
  (setq doom-modeline-bar-width 0
	doom-modeline-buffer-file-name-style 'file-name
	doom-modeline-icon t
	doom-modeline-position-column-line-format '("%l:%c")
	doom-modeline-buffer-encoding nil
					;doom-modeline-percent-position nil
	)

  ;; Prevent icons from rendering bold in the modeline.
  ;; doom-modeline-propertize-icon sets :inherit, :family, :height on icons
  ;; but inherits :weight from faces like doom-modeline-info (bold).
  ;; This advice adds :weight normal so icon glyphs don't get synthetic bold.
  (advice-add 'doom-modeline-propertize-icon :filter-return
              (lambda (icon)
                (when (stringp icon)
                  (let ((face (get-text-property 0 'face icon)))
                    (when face
                      (put-text-property 0 (length icon) 'face
                                         (append face '(:weight normal)) icon))))
                icon))
  )

;; ==============================================================================
;; Completion
;; ==============================================================================

;; ==============================================================================
;; Hydra
;; ==============================================================================

(use-package hydra)
(use-package pretty-hydra)

;; ==============================================================================
;; Completion
;; ==============================================================================

(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  :config
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))
  )

(use-package vertico-directory
  :after vertico
  :ensure nil
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-buffer :after vertico :ensure nil )
(use-package vertico-indexed :after vertico :ensure nil)
(use-package vertico-flat :after vertico :ensure nil)
(use-package vertico-grid :after vertico :ensure nil)
(use-package vertico-unobtrusive :after vertico :ensure nil)
(use-package vertico-multiform
  :ensure nil
  :after vertico 
  :config
  (vertico-multiform-mode 1)

  ;; (setq vertico-multiform-commands
  ;; 	'((consult-imenu buffer)
  ;;         (project-find-file buffer)
  ;;         ;;("persp-.*" flat)
  ;;         ))

  ;; (setq vertico-multiform-categories
  ;; 	'((consult-grep buffer)))
  )

;; Reopen the last minibuffer session with all input and selection preserved.
(use-package vertico-repeat
  :ensure nil
  :commands (vertico-repeat vertico-repeat-save)
  :hook (minibuffer-setup . vertico-repeat-save))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure nil
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :ensure nil
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '(
				   (file (styles orderless basic partial-completion))
				   (buffer (styles orderless basic partial-completion))
				   (project-file (styles orderless))
				   (symbol (styles orderless-case-insensitive))
				   (function (styles orderless-case-insensitive))))
  :config
  (setq completion-ignore-case t)
  (setq orderless-smart-case t)
  (orderless-define-completion-style orderless-case-insensitive
    (orderless-matching-styles '(orderless-literal orderless-regexp))
    (orderless-smart-case nil))
  ;; Enable per-component style switching via prefix characters:
  ;;   !term — exclude matches    =term — literal only
  ;;   ~term — flex match         ,term — initialism
  ;;   &term — match annotation (e.g. file path in marginalia)
  (setq orderless-style-dispatchers '(orderless-affix-dispatch)))

(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :ensure t
  :demand t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :demand t
  :general 
  (
   :prefix "SPC"
   :states '(normal)
   :keymaps 'override
   "sb" 'consult-line
   "si" 'consult-imenu
   )
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-fd)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)


  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any)
   ;; Hint that narrowing is available when switching buffers.
   consult-buffer :prompt "Buffer [< narrow, ! exclude, = literal, ~ flex, , initialism, & annotation]: "
   ;; Hint about filtering options in ripgrep.
   consult-ripgrep :prompt "Ripgrep [# filter, -- rg flags, ! exclude, = literal, ~ flex, , initialism, & annotation]: ")

  ;; Remove the redundant "Buffer" group header in consult-buffer
  ;; (it's the only group shown by default, so the label is noise).
  (plist-put consult-source-buffer :name nil)
  ;; Scope consult-buffer to the current perspective's buffers.
  (plist-put consult-source-buffer :items
             (lambda () (consult--buffer-query :sort 'visibility
                                              :as #'buffer-name
                                              :predicate #'persp-is-current-buffer)))
  ;; Scope recent files to the current project root, rename for clarity,
  ;; and use 'r' as the narrow key.
  (plist-put consult-source-recent-file :name "Recent")
  (plist-put consult-source-recent-file :narrow ?r)
  (plist-put consult-source-recent-file :items
             (lambda ()
               (when-let ((root (and (project-current) (project-root (project-current)))))
                 (cl-remove-if-not (lambda (f) (string-prefix-p root f))
                                   recentf-list))))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
   ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
   ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
   ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
   ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
   ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )

(use-package consult-atuin
  :ensure nil
  :load-path "lisp/"
  :after consult)

(use-package embark
  :demand t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-S-." . embark-act-all)   ;; act on all filtered candidates
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  (evil-define-key 'normal 'global (kbd "C-.") #'embark-act)
  (evil-define-key 'normal org-mode-map (kbd ",") #'embark-act)

  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
 The which-key help message will show the type and value of the
 current target followed by an ellipsis if there are further
 targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
	(which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  (setq embark-indicators
	'(embark-which-key-indicator
	  embark-highlight-indicator
	  embark-isearch-highlight-indicator))

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator)

  ;; Fix: which-key's idle timer fires during embark actions that open
  ;; a minibuffer (e.g. org-schedule), clobbering it with a which-key
  ;; popup. Stop the timer during action execution and restart after.
  ;; Fix: which-key's idle timer fires during embark actions that open
  ;; a minibuffer (e.g. org-schedule), clobbering it with a which-key
  ;; popup. Stop the timer during action execution and restart after.
  (advice-add #'embark--act :around
              (lambda (fn &rest args)
                (which-key--stop-timer)
                (which-key--hide-popup-ignore-command)
                (unwind-protect (apply fn args)
                  (which-key--start-timer))))

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
					; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview. When using insert, eglot also inserts type info we don't want.
  (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :bind
  ;; Use TAB for cycling, default is `corfu-complete'.
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)) 
  :init
  ;; Auto-popup completion candidates as you type in all buffers
  (global-corfu-mode)
  :config
  (dolist (c (list (cons "SPC" " ")
		   (cons "." ".")
		   (cons "," ",")
		   (cons ":" ":")
		   (cons ")" ")")
		   (cons "}" "}")
		   (cons "]" "]")))
    (define-key corfu-map (kbd (car c)) `(lambda ()
					   (interactive)
					   (corfu-insert)
					   (insert ,(cdr c)))))

  ;; Remember completion choices so frequently used candidates rank higher.
  (corfu-history-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history)

  ;; Show documentation popup for the selected candidate.
  (corfu-popupinfo-mode 1)
  (setq corfu-popupinfo-delay '(0.4 . 0.2)))

;; Richer help buffers with source code, references, and callees.
(use-package helpful
  :commands (helpful-callable helpful-variable helpful-key helpful-command)
  :init
  (global-set-key [remap describe-function] #'helpful-callable)
  (global-set-key [remap describe-variable] #'helpful-variable)
  (global-set-key [remap describe-key] #'helpful-key)
  (global-set-key [remap describe-command] #'helpful-command)
  :config
  ;; Evil's jump list ignores non-file buffers by default.
  ;; Include helpful buffers so C-o returns to them.
  (setq evil--jumps-buffer-targets "\\*\\(new\\|scratch\\|helpful .*\\)\\*")
  ;; Skip the expensive Info manual lookup (saves ~90% of memory
  ;; allocation). Only cost is losing the "View in manual" link.
  (advice-add 'helpful--in-manual-p :override (lambda (&rest _) nil))
  ;; Set evil jump points when entering and navigating helpful buffers.
  (dolist (cmd '(helpful-callable helpful-variable helpful-key helpful-command))
    (advice-add cmd :before (lambda (&rest _) (evil-set-jump))))
  (advice-add 'push-button :before
              (lambda (&rest _)
                (when (derived-mode-p 'helpful-mode)
                  (evil-set-jump)))))

;; A few more useful configurations...
(use-package emacs
  :ensure nil
  :custom
  ;; TAB cycle if there are only few candidates
  (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
					;(tab-always-indent 'complete)
  (tab-always-indent t)

  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  (text-mode-ispell-word-completion nil)

  ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current
  ;; mode.  Corfu commands are hidden, since they are not used via M-x. This
  ;; setting is useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

;; Completion At Point Extensions - adds extra completion backends to corfu:
;; cape-dabbrev: complete from words in open buffers (scoped to current perspective)
;; cape-file: complete file paths anywhere
(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  :config
  ;; Only search buffers in the current perspective workspace for dabbrev completions
  (setq dabbrev-friend-buffer-function
        (lambda (buf)
          (memq buf (persp-buffers (persp-curr))))))

;; Edit grep/ripgrep results in-place and save back to files.
;; Workflow: consult-ripgrep -> C-. E (embark-export) -> C-c C-p (wgrep) -> edit -> C-c C-c (save)
(use-package wgrep)

;; ==============================================================================
;; Git
;; ==============================================================================

(use-package transient)

(use-package cond-let
    :ensure (:host github :repo "tarsius/cond-let")
    :config
    (require 'cond-let)
    )

(use-package magit
  :after (transient compat cond-let)
  :commands (magit-status)
  :custom (magit-bury-buffer-function #'magit-restore-window-configuration)
  :init
  ;; Define the prefix keymap eagerly in :init so SPC g works before magit loads.
  ;; magit-status/magit-blame are autoloaded by magit, so pressing SPC g s
  ;; will trigger magit to load on first use.
  (general-define-key
   :prefix-command 'magit-keys
   "s" 'magit-status
   "b" 'magit-blame
   "B" 'magit-blame-quit)
  :config
  (define-key transient-map        (kbd "<escape>") 'transient-quit-one)
  (define-key transient-edit-map   (kbd "<escape>") 'transient-quit-one)
  (define-key transient-sticky-map (kbd "<escape>") 'transient-quit-seq)

  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

  (defun auto-display-magit-process-buffer (&rest args)
    "Automatically display the process buffer when it is updated."
    (let ((magit-display-buffer-noselect t))
      (magit-process-buffer)))

  (advice-add 'magit-process-set-mode-line-error-status :before 
	      #'auto-display-magit-process-buffer)  
  )


(use-package autorevert
  :diminish auto-revert-mode
  :ensure nil
  :config
  (global-auto-revert-mode 1)
  (setq auto-revert-check-vc-info nil)
  (setq auto-revert-interval 1)
  )

(use-package git-link
  :commands git-link)

;; Show git diff indicators in the margin (added/modified/deleted lines).
;; Uses margin mode because fringes are disabled.
;; Magit hooks ensure indicators update after staging/unstaging.
(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (diff-hl-margin-mode)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))



;; ==============================================================================
;; Motion
;; ==============================================================================

(use-package avy
  :config
  (defun avy-goto-char-flex (char1 char2 &optional arg beg end)
    "Behaves like avy-goto-char-2 unless the second character is RET,
in which case does avy-goto-char with the first char."
    (interactive (list (read-char "char 1: " t)
                       (read-char "char 2: " t)
                       current-prefix-arg
                       nil nil))
    (when (eq char1 ?
	      )
      (setq char1 ?\n))
    (if (not (eq char2 ?
		 ))
	(avy-with avy-goto-char-2
	  (avy-jump
	   (regexp-quote (string char1 char2))
	   :beg beg
	   :end end))
      (avy-with avy-goto-char
	(avy-jump
	 (regexp-quote (string char1))))))

  (setq avy-timeout-seconds 0.15)
  (setq avy-all-windows nil)
  (setq avy-keys '(?h ?u ?t ?e ?d ?i ?s ?a ?g ?p ?c ?. ?f ?y ?r ?, ?l ?' ?m
		      ?k ?w ?j ?b ?x ?v ?q ?z ?\; ?n ?o))
					; (setq avy-keys '("hutedisagpc.fyr,l'mkwjbxvqz\;no"))

  (set-face-attribute 'avy-lead-face nil
		      :background 'unspecified
		      :foreground "#ff0000"
		      )

  (set-face-attribute 'avy-lead-face-0 nil
		      :background 'unspecified
		      :foreground "#ff0000"
		      )

  (setq avy-background t)
  (setq avy-all-windows t)

  )

(use-package flash
  :ensure (:host github :repo "ftzm/flash" :branch "overlay-expand")
  :commands (flash-jump flash-treesitter)
  :bind ("s-j" . flash-jump)
  :init
  (with-eval-after-load 'evil
    (require 'flash-evil))
  :config
  (require 'flash-isearch)
  (flash-isearch-mode 1)
  (set-face-attribute 'flash-label nil                                                                           
                      :background 'unspecified                                                                   
                      :foreground "#ff0000")                                                                     
  (setq flash-labels "hutedisagpc.fyr,l'mkwjbxvqz;no")
  (setq flash-multi-char-labels t)
  (setq flash-label-position 'overlay-expand))

;; ==============================================================================
;; Project and Perspective
;; ==============================================================================

(use-package perspective
  :config
  (setq persp-mode-prefix-key (kbd "C-c M-p"))  ; pick your own prefix key here
  (persp-mode)

  ;; Override persp-mode-line to hide in global-mode-string
  (defun persp-mode-line () "")

  (setq fixed-persp-names '("mail"))

  (defun hydra-persp-names ()
    (let ((persps (sort (hash-table-values (perspectives-hash))
			(lambda (a b)
			  (string< (persp-name a)
				   (persp-name b)))))
	  (current-name (persp-current-name))
	  (parts '())
	  (count 1))
      (dolist (persp persps (s-join "\n " (nreverse parts)))
	(let ((name (persp-name persp))
	      (bufcount (length (seq-filter (lambda (b)
					      (and (buffer-live-p b)
						   (not (minibufferp b))))
					    (persp-buffers persp)))))
	  (cond ((eq name current-name)
		 (push (propertize (format "%d %s (%d)" count name bufcount) 'face
				   'font-lock-warning-face) parts))
		(t (push (format "%d %s (%d)" count name bufcount) parts))))
	(cl-incf count))))

  (defun persp-switch-label (n)
    (nth (- n 1) (persp-names)))

  (defun persp-switch-command (n)
    (persp-switch (nth (- n 1) (persp-names))))

  (pretty-hydra-define persp-hydra
    (:color blue :quit-key "q" :title "%s(hydra-persp-names)")
    ("Buffer"
     (( "k" (persp-remove-buffer (current-buffer)) "remove buffer")
      ( "a" (persp-add-buffer (current-buffer)) "remove buffer")
      ( "i" persp-ibuffer "ibuffer"))
     "Persp"
     (("r" persp-rename "rename")
      ("C" (persp-kill (persp-current-name)) "kill"))
     "General"
     (("s" persp-switch "switch")
      ("p" persp-switch-last (format "prev [%s]"
				     (if (persp-last)
					 (persp-name (persp-last))
				       persp-initial-frame-name)))
      ("1" (persp-switch-command 1) nil)
      ("2" (persp-switch-command 2) nil)
      ("3" (persp-switch-command 3) nil)
      ("4" (persp-switch-command 4) nil)
      ("5" (persp-switch-command 5) nil)
      ("6" (persp-switch-command 6) nil)
      ("7" (persp-switch-command 7) nil)
      ("8" (persp-switch-command 8) nil)
      ("7" (persp-switch-command 7) nil))))

  )

(use-package project
  :ensure nil
  :after general
  :config
  (project--read-project-list)
  (defun switch-persp-project ()
    (interactive "")
    (let ((projects project--list))
      (if projects (let* ((project
			   (completing-read "Project: " projects))
			  (persp-name
			   (file-name-nondirectory (substring project 0 -1))))
		     (persp-switch persp-name)
		     (project-switch-project project))
	(user-error "There are no known projects"))))

  (general-define-key
   :prefix-command 'project-keys
   "f" 'project-find-file
					;"F" 'projectile-find-file-other-window
   "b" 'project-switch-to-buffer
   "p" 'switch-persp-project
   "P" 'project-switch-project
   "g" 'consult-ripgrep
					;"t" 'projectile-run-vterm
   )
  )

;; ==============================================================================
;; Env
;; ==============================================================================

(use-package direnv
  :config
  (advice-add 'lsp :before #'direnv-update-environment)
  (direnv-mode)
  )

;; ==============================================================================
;; LSP
;; ==============================================================================

(use-package eglot
  :ensure nil
  :after general
  :hook (((js-ts-mode json-ts-mode yaml-ts-mode typescript-ts-mode java-ts-mode mhtml-mode css-ts-mode vue-ts-mode haskell-mode nix-ts-mode scala-mode php-mode phps-mode jsonnet-mode) . eglot-ensure))
  :preface
  (defun vue-eglot-init-options ()
    (let ((tsdk-path (expand-file-name
                      "lib"
                      (string-trim-right (shell-command-to-string "npm list --global --parseable typescript | head -n1")))))
      `(:typescript (:tsdk ,tsdk-path
                           :languageFeatures (:completion
                                              (:defaultTagNameCase "both"
    								   :defaultAttrNameCase "kebabCase"
    								   :getDocumentNameCasesRequest nil
    								   :getDocumentSelectionRequest nil)
                                              :diagnostics
                                              (:getDocumentVersionRequest nil))
                           :documentFeatures (:documentFormatting
                                              (:defaultPrintWidth 100
    								  :getDocumentPrintWidthRequest nil)
                                              :documentSymbol t
                                              :documentColor t)))))
  :config
					; disable snippets in completion
  (fset #'eglot--snippet-expansion-fn #'ignore)

  ;; disable inlay hints by default
  (add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode -1)))

  ;; Always auto-reconnect when an LSP server crashes
  (setq eglot-autoreconnect t)

  ;; Auto-reconnect eglot after buffer reverts from external file changes.
  ;; Debounced to avoid hammering the LSP server when multiple files change at once.
  (defvar my/eglot-reconnect-timer nil)
  (defun my/eglot-reconnect-after-revert ()
    (when (and (bound-and-true-p eglot--managed-mode)
               (eglot-current-server))
      (when my/eglot-reconnect-timer
        (cancel-timer my/eglot-reconnect-timer))
      (setq my/eglot-reconnect-timer
            (run-with-idle-timer
             2 nil
             (lambda ()
               (let ((server (eglot-current-server)))
                 (when server
                   (ignore-errors (eglot-reconnect server))))
               (setq my/eglot-reconnect-timer nil))))))
  (add-hook 'after-revert-hook #'my/eglot-reconnect-after-revert)

  (add-to-list 'eglot-server-programs
               `(vue-ts-mode . ("vue-language-server" "--stdio" :initializationOptions ,(vue-eglot-init-options))))

  (add-to-list 'eglot-server-programs '(nix-ts-mode . ("nil" :initializationOptions (:formatting (:command ["alejandra"])))))

  (add-to-list 'eglot-server-programs '(scala-mode . ("metals" :initializationOptions (:metals (:startMcpServer t)))))

  (setq-default eglot-workspace-configuration
		'(:metals (:startMcpServer t)))


  (add-to-list 'eglot-server-programs
               '((php-mode phps-mode) . ("intelephense" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(haskell-mode . ("haskell-language-server-wrapper" "--lsp"
                                 :initializationOptions
                                 (:haskell (:formattingProvider "fourmolu")))))
  (add-to-list 'eglot-server-programs '(jsonnet-mode . ("jsonnet-language-server")))

  (my-leader-def
    :states '(normal)
    :keymaps 'eglot-mode-map
    "en" 'flymake-goto-next-error
    "ep" 'flymake-goto-prev-error
    "eb" 'consult-flymake)
  (evil-define-key 'normal 'global (kbd "gi") 'eglot-find-implementation)
  )

(use-package eglot-booster
  :ensure (:host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config
  (eglot-booster-mode))

;; Workspace-wide symbol search via LSP. Type a query, then # to
;; switch to client-side orderless filtering of the results.
(use-package consult-eglot
  :after (consult eglot)
  :commands (consult-eglot-symbols)
  :config
  (consult-customize consult-eglot-symbols
                     :prompt "LSP Symbol [# to filter]: "))

;; Debug Adapter Protocol client (same protocol as VS Code debuggers).
;; M-x dape to start, M-x dape-breakpoint-toggle for breakpoints.
;; Supports C/C++ (gdb/lldb), Rust, Python, JS/TS out of the box.
(use-package dape
  :config
  (setq dape-buffer-window-arrangement 'right)  ;; debug panes on the right

  ;; UNTESTED: Metals/Scala debug adapter integration.
  ;; Asks Metals to start a debug session via LSP, then connects dape to the returned port.
  ;; Usage: M-x dape, select "metals", fill in mainClass and buildTarget.
  ;; If broken, evaluate (eglot-execute-command (eglot-current-server) "debug-adapter-start" ...)
  ;; in *scratch* to inspect the return value — the :port extraction may need adjusting.
  (add-to-list 'dape-configs
    `(metals
      modes (scala-mode)
      fn (lambda (config)
           (let* ((params `(:mainClass ,(plist-get config :mainClass)
                            :buildTarget ,(plist-get config :buildTarget)
                            :args ,(or (plist-get config :args) [])))
                  (result (eglot-execute-command
                           (eglot-current-server)
                           "debug-adapter-start"
                           (vector params)))
                  (port (plist-get result :port)))
             (plist-put config :port port)
             (plist-put config :host "localhost")
             config))
      :mainClass ""
      :buildTarget ""
      :args [])))

;; ==============================================================================
;; Language config
;; ==============================================================================

(use-package reformatter
  ;; for nix
  :init
  (reformatter-define alejandra
    :program "alejandra"
    :lighter " alej"
    )
  (reformatter-define fourmolu
    :program "fourmolu"
    :args `("--stdin-input-file" ,buffer-file-name)
    :lighter " four"
    )
  (reformatter-define pgformatter
    :program "pg_format"
    :args `("-" "-s2" "-g")
    :lighter " pgform"
    )
  (reformatter-define scalafmt
    :program "scalafmt-native"
    :args `("--stdin")
    :lighter " sfmt"
    )
  (reformatter-define cog
    :program "cog"
    :args `("-")
    :lighter " cog"
    :stdin t
    )
  (reformatter-define prettier
    :program "npx"
    :args `("prettier" "--stdin-filepath" ,buffer-file-name)
    :lighter " prettier"
    )
  )

(use-package nix-ts-mode
  :mode "\\.nix\\'"
  :hook (nix-ts-mode . alejandra-on-save-mode)
  )

(use-package haskell-mode
  :mode "\\.hs\\'"
  :hook (haskell-mode . fourmolu-on-save-mode)
  :config
  ;; Use haskell-indentation-mode and configure evil integration
  (add-hook 'haskell-mode-hook #'haskell-indentation-mode)
  (add-hook 'haskell-mode-hook (lambda () (haskell-indent-mode -1)))
  ;; Make evil-open-below use proper Haskell indentation
  (evil-define-key 'normal haskell-mode-map
    "o" (lambda ()
          (interactive)
          (end-of-line)
          (haskell-indentation-newline-and-indent)
          (evil-insert-state)))
  (evil-define-key 'normal haskell-mode-map
    "O" (lambda ()
          (interactive)
          (beginning-of-line)
          (open-line 1)
          (indent-according-to-mode)
          (evil-insert-state)))
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-safe-remote-resources
   '("\\`https://fniessen\\.github\\.io/org-html-themes/org/theme-readtheorg\\.setup\\'"))
 '(safe-local-variable-values '((dante-methods new-flake))))

(use-package c-ts-mode
  :mode (("\\.h$". c-ts-mode)("\\.c$". c-ts-mode)
	 )
  :hook (c-ts-mode . eglot-ensure)
  :ensure nil
  :config
  ;; Format on save, buffer-local so it only applies to C buffers
  (add-hook 'c-ts-mode-hook
    (lambda () (add-hook 'before-save-hook 'eglot-format nil t)))
  (evil-define-key 'normal c-mode-map (kbd ",a") 'eglot-code-actions)
  (evil-define-key 'normal c-ts-mode-map (kbd ",a") 'eglot-code-actions)
  )

(use-package rust-mode
  :mode ".rs\\'"
  :interpreter ("rust" . rust-mode)
  :hook ((rust-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs '(rust-mode . ("rust-analyzer")))
  )



(use-package real-auto-save
  :hook (org-mode . real-auto-save-mode)
  :config
  (setq real-auto-save-interval 5))

;; ==============================================================================
;; Org
;; ==============================================================================

(use-package org
  :ensure nil
  :after pretty-hydra
  :hook (
  	 (org-mode . auto-revert-mode)
					;(org-mode . yas-minor-mode)
					;(org-mode . olivetti-mode)
					;(org-mode . typo-mode)
   	 )
  :init
  (pretty-hydra-define org-global-hydra
    (:color blue :quit-key "q" :title "Org Dispatch")
    ("Agenda"
     (( "g" (org-agenda nil " ") "Personal Agenda")
      ( "wg" (org-agenda nil "W") "Work Agenda"))
     "Capture"
     (("t" (org-capture nil "t") "Task")
      ("d" (org-capture nil "d") "Diary")
      ("wt" (org-capture nil "w") "Work Task")
      ("wd" (org-capture nil "u") "Work Diary"))
     ))
  :config
  
  (setq org-todo-keywords '((sequence "NEXT(n)" "TODO(t)" "|" "DONE(d/!)" "CANCELLED(c@/!)")))
  (setq org-use-fast-todo-selection t) ; select todo via key rather than cycling
  (setq org-use-fast-todo-selection 'expert) ; show options in minibuffer

  ;; apply CLOSED property on done
  (setq org-log-done 'time)

  (setq org-startup-folded 'content)

  ;; Prevent org-todo from unfolding drawers.
  ;; When CLOSED is inserted, org-fold's fragile check
  ;; (org-fold--reveal-drawer-or-block-maybe) sees the planning line
  ;; before the drawer instead of :PROPERTIES: and unfolds it.
  (advice-add 'org-todo :around
              (lambda (orig-fn &rest args)
                (cl-letf (((symbol-function 'org-fold--reveal-drawer-or-block-maybe)
                           (lambda (&rest _) nil)))
                  (apply orig-fn args))))

  ;; ==============================================================================
  ;; Agenda

  (general-define-key
   :states '(normal motion)
   :keymaps 'org-agenda-mode-map
   "R" 'org-agenda-refile
   )

  (setq org-agenda-files '("~/org/personal/"
			   "~/org/personal/projects/" 
			   "~/org/personal/culture/" ))

  (setq org-agenda-window-setup 'current-window)

  (setq org-agenda-custom-commands
	`((" " "My Agenda"
	   ((org-sep-header "Ní dhéanfaidh smaoineamh an treabhadh dhuit\n")
	    (org-ql-block `(and (not (todo "DONE"))
				(not (habit))
				(deadline :to -1))
			  ((org-ql-block-header "\nMissed Deadlines")))
	    (org-ql-block '(and (not (todo "DONE"))
				(not (deadline :to -1))
				(deadline auto)
				(not (scheduled :from +1)))
			  ((org-ql-block-header "\nDeadlines")))
	    (org-ql-block '(and (scheduled :to today)
	  			(not (habit)))
	  		  ((org-ql-block-header "\nToday")))
	    (org-ql-block '(and (scheduled :to today)
	  			(habit))
	  		  ((org-ql-block-header "\nHabits")
			   ))
	    (org-ql-block '(and (todo "NEXT")
				(not (scheduled)))
	  		  ((org-agenda-files '("~/org/personal/todo.org"))
			   (org-ql-block-header "\nAd Hoc")))
	    (org-ql-block '(todo "TODO" "NEXT")
	  		  ((org-agenda-files '("~/org/personal/inbox.org"))
			   (org-ql-block-header "\nTo Refile")))
	    (org-ql-block '(todo "NEXT")
			  ((org-ql-block-header "\nProjects")
			   (org-agenda-files '("~/org/personal/projects/"))
			   (org-super-agenda-groups '((:auto-category-and-outline-path t)))))
	    (org-ql-block '(closed 0)
			  ((org-ql-block-header "\nClosed")))
	    ))))

  (defun ftzm/daily-agenda()
    (interactive)
    (org-agenda nil " "))
  
  ;; Save org buffers when quitting agenda buffer
  (advice-add 'org-agenda-quit :before (lambda () (let ((inhibit-message t)) (org-save-all-org-buffers))))

  ;; ==============================================================================
  ;; capture

  ;; start capture in insert mode
  (add-hook 'org-capture-mode-hook 'evil-insert-state)

  (setq org-capture-templates
	(quote (("t" "todo" entry (file "~/org/personal/inbox.org")
		 "* NEXT %?")
		("d" "diary entry" entry (file+datetree "~/org/personal/diary.org")
		 "* %?")
		("u" "work diary entry" entry (file+datetree "~/org/work/diary.org")
		 "* %?")
		("w" "work todo" entry (file "~/org/work/work-inbox.org")
		 "* NEXT %?")
		)))

  (defun add-property-with-created-date ()
    "Add DATE_CAPTURED property to the current item."
    (interactive)
    (org-set-property "CREATED" (format-time-string "%F")))
  
  (add-hook 'org-capture-before-finalize-hook 'add-property-with-created-date)

  ;; ==============================================================================
  ;; refile

  ;; Exclude DONE state tasks from refile targets
  (defun bh/verify-refile-target ()
    "Exclude todo keywords with a done state from refile targets"
    (not (member (nth 2 (org-heading-components)) org-done-keywords)))

  (setq org-refile-target-verify-function 'bh/verify-refile-target)

  ;; allow refiling 9 levels deep
  (setq org-refile-targets '((nil :maxlevel . 9)
			     (org-agenda-files :maxlevel . 9)))

  ;; Refile in a single go
  (setq org-outline-path-complete-in-steps nil)

  ;; show full path for refiling, preceeded by the filename.
  (setq org-refile-use-outline-path 'file)

  ;; ==============================================================================
  ;; Archive

  ;; Dynamic archive location: mirror source path under ~/org/archive/
  ;; e.g. ~/org/personal/todo.org -> ~/org/archive/personal/todo.org_archive
  (defun ftzm/org-archive-location ()
    (let* ((source (buffer-file-name))
           (org-root (expand-file-name "~/org/"))
           (relative (file-relative-name source org-root))
           (archive-file (expand-file-name (concat "archive/" relative "_archive") org-root))
           (archive-dir (file-name-directory archive-file)))
      (unless (file-directory-p archive-dir)
        (make-directory archive-dir t))
      (concat archive-file "::")))

  (defun ftzm/set-org-archive-location (&rest _)
    (when (buffer-file-name)
      (setq-local org-archive-location (ftzm/org-archive-location))))

  (advice-add 'org-archive-subtree :before #'ftzm/set-org-archive-location)

  ;; Bulk archive: DONE/CANCELLED tasks closed more than 14 days ago
  (defun ftzm/days-ago (n)
    (time-subtract (current-time) (seconds-to-time (* n 86400))))

  (defun ftzm/archive-if-old ()
    (let* ((props (org-entry-properties (point)))
           (closed-string (cdr (assoc "CLOSED" props)))
           (closed (and closed-string (date-to-time closed-string)))
           (cutoff (ftzm/days-ago 14)))
      (when (and closed (time-less-p closed cutoff))
        (org-archive-subtree)
        (setq org-map-continue-from (outline-previous-heading))
        t)))

  (defun ftzm/org-archive-old-tasks ()
    (interactive)
    (let ((count 0))
      (org-map-entries
       (lambda ()
         (when (ftzm/archive-if-old)
           (setq count (1+ count))))
       "/DONE|CANCELLED"
       'agenda)
      (org-save-all-org-buffers)
      (format "org-archive: archived %d task(s)" count)))

  ;; ==============================================================================
  ;; Keys

  (pretty-hydra-define org-global-hydra
    (:color blue :quit-key "q" :title "Org Dispatch")
    ("Agenda"
     (( "g" (org-agenda nil " ") "Personal Agenda")
      ( "wg" (org-agenda nil "W") "Work Agenda"))
     "Capture"
     (("t" (org-capture nil "t") "Task")
      ("d" (org-capture nil "d") "Diary")
      ("wt" (org-capture nil "w") "Work Task")
      ("wd" (org-capture nil "u") "Work Diary"))
     ))

  (my-leader-def
    :states '(normal)
    :states 'override
    "o" 'org-global-hydra
    )

  )

(use-package org-super-agenda
  :after org
  :config (org-super-agenda-mode)
  
  ;; unbreak evil-org keys in super agenda headings
  (setq org-super-agenda-header-map (make-sparse-keymap))
  
  ;; group by both category (usually file name) and path
  (org-super-agenda--def-auto-group category-and-outline-path "category and outline paths"
				    :key-form (org-super-agenda--when-with-marker-buffer
					       (org-super-agenda--get-marker item)
  					       (format "%s: %s" (org-get-category) (s-join "/" (org-get-outline-path)))))
  
  (defun org-sep-header (text)
    "insert a header containing `text` into an org agenda buffer"
    (let ((buffer-read-only nil))
      (insert
       (propertize
  	text
  	'face (list :foreground (face-attribute
  				 'font-lock-function-name-face :foreground) :weight 'bold)))))
  
  (setq org-super-agenda-header-separator "\n")
  
  ;; no lines between agenda blocks
  (setq org-agenda-block-separator nil)
  
  
  )

(use-package org-ql)

;; Auto-toggle org markup visibility at cursor.
;; Away from cursor: *bold* renders as bold. On cursor: raw markers appear for editing.
;; Works for emphasis (*bold*, /italic/), links, and sub/superscripts.
(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autoemphasis t)
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t))

;; ==============================================================================
;; MPD
;; ==============================================================================

(use-package mpdel
  :commands (mpdel-mode mpd-hydra/body)
  :config
  ;;; Override Playlist appearance

  (defun mpdel-tablist--song-format ()
    "Return `tabulated-list-format' value for songs."
    (vector (list "Title" 50 t)
            (list "#" 3 nil)
            (list "Album" 40 t)
            (list "Artist" 0 t)))

  (navigel-method mpdel navigel-entity-to-columns ((song libmpdel-song))
		  (vector
		   (propertize (or (libmpdel-entity-name song) "") 'face 'mpdel-tablist-song-name-face)
		   (propertize (or (libmpdel-song-track song) "") 'face 'mpdel-tablist-track-face)
		   (propertize (or (libmpdel-album-name song) "") 'face 'mpdel-tablist-album-face)
		   (propertize (or (libmpdel-artist-name song) "") 'face 'mpdel-tablist-artist-face)))

					; (use-package ivy-mpdel
					; :straight t)

  (defun ftzm-mpd-composer-search (name)
    (mpdel-core-search-by-filter (format "(composer == '%s')" name)))

  (defun ftzm-mpd-composer-findadd (name)
    (libmpdel-send-command (format "findadd \"(composer == '%s')\"" name)))

  ;; (defun ftzm-ivy-composer-search ()
  ;;   (interactive)
  ;;   (libmpdel-send-command
  ;;    "list composer"
  ;;    (lambda (data)
  ;;      (ivy-read "Composer: " (mapcar (lambda (x)
  ;; 				      (cdr x)) data)
  ;; 	       :action (lambda (x)
  ;; 			 (ftzm-mpd-composer-search x))
  ;; 	       :caller 'composer-names))))


  (defun ftzm-embark-composer-search ()
    (interactive)
    (libmpdel-send-command
     "list composer  \"(Genre == \\\"Classical\\\")\""
     (lambda (data)
       (with-local-quit ;; I can't remember why this was necessary, maybe it's not?
         (ftzm-mpd-composer-search
	  (consult--read (seq-filter (lambda (x) (> (length x) 0)) (mapcar (lambda (x) (cdr x)) data))
		         :prompt "Composer: "
		         :category 'composer
		         ))))))

  (defun ftzm-embark-artist-search ()
    (interactive)
    (libmpdel-send-command
					;"list artist  \"(Genre == \\\"Classical\\\")\""
     "list artist"
     (lambda (data)
       (with-local-quit
         (mpdel-core-search-by-artist
	  (consult--read (seq-filter (lambda (x) (> (length x) 0)) (mapcar (lambda (x) (cdr x)) data))
		         :prompt "Artist: "
		         :category 'artist
			 :sort nil
		         ))))))

  (defun prepare-candidates (data)
    (seq-filter (lambda (x) (> (length x) 0)) (mapcar (lambda (x) (cdr x)) data)))

  (defun select-candidate-callback (candidates prompt category)
    ;; `with-local-quit` is necessary to avoid getting an error when quitting
    ;; the completion in a callback
    (consult--read (prepare-candidates candidates)
		   :prompt prompt
		   :category 'category))

  (defun list-non-classical-artists (fun)
    (with-local-quit
      (libmpdel-send-command "list artist \"(Genre != \\\"Classical\\\")\"" fun)))

  (defun list-artist-albums (artist fun)
    (with-local-quit
      (libmpdel-send-command
       (format  "list album \"(Artist == \\\"%s\\\")\"" artist)
       fun)))

  (require 'libmpdel)
  (defun ftzm-embark-artist-album-search ()
    (interactive)
    (list-non-classical-artists
     (lambda (artist-data)
       (let ((artist-name (select-candidate-callback artist-data "Artist: " 'artist)))
	 (list-artist-albums
	  artist-name
	  `(lambda (album-data)
	     (with-local-quit
	       (let* ((album-name (select-candidate-callback album-data "Album: " 'album))
		      (artist (libmpdel--artist-create :name ,artist-name))
		      (album (libmpdel--album-create :name album-name :artist artist))
		      )
		 (libmpdel-playlist-add album 'current-playlist)
					;	;(mpdel-core-search-by-album)
		 ))))))))

  (defun climb-entries (f)
    (catch 'exit
      (save-excursion
	(while t
	  (funcall f)
          ;; if we are at top level, return nil
          (when (= (org-outline-level) 1)
            (throw 'exit nil))
          ;; if we cannot go up any further, return nil
          (when (not (outline-up-heading 1))
            (throw 'exit nil))))))

  (defun entries-from-top ()
    (let ((acc (list)))
      (climb-entries '(lambda () (push (org-entry-properties) acc)))
      acc))

  (defun assoc-to-query-string (key val)
    (format
     (pcase key
       ("album" "(album == '%s')")
       ("artist" "(artist contains '%s')")
       ("composer" "(composer == '%s')")
       ("work" "(title contains '%s')"))
     val))

  (defun construct-request ()
    (interactive)
    (let* ((text-quoting-style 'straight)
	   (entries (entries-from-top))
	   (fields (split-string (cdr (assoc "FIELDS" (car entries))) " "))
	   (titles (mapcar (lambda (x) (cdr (assoc "ITEM" x)))  (cdr entries)))
	   (pairs (cl-mapcar (lambda (x y) `(,x . ,y)) fields titles))
	   (inner-query (string-join
			 (mapcar (lambda (p)
				   (assoc-to-query-string (car p) (cdr p))) pairs) " AND "))
	   (query (format "(%s)" inner-query)))

      (mpdel-core-search-by-filter (message (format "%s" query)))
      ;;(mpdel-core-search-by-filter "((composer == 'Lucien Durosoir'))")
      ))

  (defun ftzm-mpd-format-time (data)
    (format "%s / %s" (libmpdel-time-to-string (cdr (assq 'elapsed data)))
	    (libmpdel-time-to-string (cdr (assq 'duration data)))))

  (defun ftzm-mpd-get-play-time ()
    (let* ((output "null")
	   (to-wait 100)
	   (was-set nil)
	   (watch-func '(lambda
			  (&rest
			   args)
			  (setq was-set t))))
      (add-variable-watcher 'output watch-func)
      (libmpdel-send-command "status" (lambda (data)
				        (setq output (ftzm-mpd-format-time data))))
      (while (and (not was-set)
		  (> to-wait 0))
        (setq to-wait (1- to-wait))
        (sit-for 0.001))
      (if (< to-wait 1)
	  (setq output "~"))
      (remove-variable-watcher 'output watch-func) output))

					;(remove-variable-watcher 'output (lambda (&rest args) (setq was-set t)))
					;(get-variable-watchers 'output)

  (defun ftzm-mpd-status ()
    (if libmpdel--current-song
        (let ((play-time (ftzm-mpd-get-play-time))
	      (playing (libmpdel-play-state))
	      (current-song (if libmpdel--current-song (libmpdel--song-name
						        libmpdel--current-song) ""))
	      (artist (if libmpdel--current-song  (libmpdel--artist-name
						   (car (libmpdel--album-artists
							 (libmpdel--song-album
							  libmpdel--current-song)))) "")))
	  (format "[%s] %s \n %s\n %s" playing play-time current-song artist))
      "No song selected."))

  (defun ftzm-mpd-repeat ()
    (interactive)
    (if libmpdel--repeat
        (libmpdel-playback-unset-repeat)
      (libmpdel-playback-set-repeat)))

  (defun ftzm-mpd-single ()
    (interactive)
    (if (eq 'forever libmpdel--single)
        (libmpdel-playback-set-single-never)
      (libmpdel-playback-set-single-forever)))

  (defun ftzm-mpd-random ()
    (interactive)
    (if libmpdel--random
        (libmpdel-playback-unset-random)
      (libmpdel-playback-set-random)))

  (defun ftzm-mpd-clear-playlist ()
    (interactive)
    (libmpdel-current-playlist-replace '()))

  (pretty-hydra-define mpd-hydra
    (:color blue
	    :quit-key "q"
	    :title (ftzm-mpd-status))
    ("Playback" (( "p" libmpdel-playback-play-pause "play-pause")
	         ( "<" mpdel-song-normal-decrement "skip backward" :exit nil)
	         ( ">" mpdel-song-normal-increment "skip forward" :exit nil)
	         ( "{" libmpdel-playback-previous "previous" :exit nil)
	         ( "}" libmpdel-playback-next "next" :exit nil))
     "Selection" (( "s" mpdel-playlist-open "open current playlist")
		  ( "C" ftzm-mpd-clear-playlist "clear playlist")
		  ( "a" ftzm-embark-artist-search "search artists")
		  ( "c" ftzm-embark-composer-search "search composers")
					;( "a" ivy-mpdel-artists "search artists")
		  )
     "Toggles" (( "r" ftzm-mpd-repeat "repeat" :toggle (symbol-value 'libmpdel--repeat))
	        ( "S" ftzm-mpd-single "single" :toggle (eq 'forever libmpdel--single))
	        ( "R" ftzm-mpd-random "random" :toggle (symbol-value 'libmpdel--random)))))


  (evil-collection-define-key 'normal 'mpdel-core-map
    "m"  'tablist-mark-forward)

  (defvar-keymap embark-composer-actions
    :doc "Keymap for actions on composers."
    :parent embark-general-map
    "s" #'ftzm-mpd-composer-search
    "r" #'ftzm-mpd-composer-findadd)
  
  (assq-delete-all 'composer embark-keymap-alist)
  (add-to-list 'embark-keymap-alist '(composer . embark-composer-actions)))

;; ==============================================================================
;; Lisp
;; ==============================================================================

(use-package racket-mode)

(use-package geiser
  :defer t)

(use-package geiser-chez
  :after geiser)

;; Defensive auto-heal for stale-session load errors -- KEPT, BUT POSSIBLY DEAD
;; CODE.  The theory: Chez caches each imported library per session, so editing
;; a library's exports and then loading a dependent could bind to a stale
;; resident copy and fail with an unbound-identifier or "different compilation
;; instance" error even though the source is correct; the cure is a clean
;; reload from a fresh process.
;;
;; Caveat learned from testing (2026-06): I could NOT reproduce that failure
;; through geiser's actual save-hook load path.  `geiser:load-file' uses
;; `compile-and-load' (compile-file + load), which recompiles the target and
;; re-reads dependency *source* on every load -- so it already self-heals.  The
;; per-session staleness only showed up with raw interactive `(import ...)' at
;; the REPL, not via this hook.  So this code may never fire on a save; it is
;; kept only as a cheap safety net for staleness introduced by interactive
;; imports.  It is at least harmless: a genuine error survives the clean
;; restart, so we cache its text and stop -- one restart the first time an
;; error appears, none thereafter, never a loop (this bound is unit-tested).
(defconst scheme-ts--stale-error-rx
  (rx (or "different compilation instance"
          "unbound identifier"
          "is not bound"
          "not bound"
          "not visible in"))
  "Load-error signatures that may be stale-session artifacts worth one reload.")

(defvar-local scheme-ts--last-confirmed-error nil
  "Text of the last load error confirmed genuine by surviving a clean restart.
While the same error persists we skip the restart instead of thrashing.")

(defun scheme-ts--retort-error-text (ret)
  "Combined error+output text of geiser load retort RET, or nil if it succeeded."
  (let ((err (geiser-eval--retort-error ret)))
    (when err
      (string-trim
       (format "%s %s"
               (let ((m (geiser-eval--error-msg err)) ) (if m (format "%s" m) ""))
               (or (geiser-eval--retort-output ret) ""))))))

(defun scheme-ts--load-file-code ()
  "Geiser eval code that loads the current buffer's file."
  (list :load-file (file-local-name (buffer-file-name))))

(defun scheme-ts-auto-load-on-save (&optional buf)
  "Load BUF (default current) into the Chez REPL, healing stale-session errors.
Loads asynchronously; on a stale-looking failure, restarts the REPL once and
reloads from a clean slate (see `scheme-ts--stale-error-rx')."
  (let ((buf (or buf (current-buffer))))
    (when (and (buffer-live-p buf)
               (buffer-file-name buf)
               (geiser-repl--connection*))
      (with-current-buffer buf
        (let ((label (format "Loading %s" (file-local-name (buffer-file-name)))))
          (geiser-autodoc--clean-cache)
          (message "%s ..." label)
          (geiser-eval--send
           (scheme-ts--load-file-code)
           (lambda (ret) (scheme-ts--after-load buf label ret))))))))

(defun scheme-ts--after-load (buf label ret)
  "Handle the first (async) load result RET for BUF, labelled LABEL."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let ((err (scheme-ts--retort-error-text ret)))
        (cond
         ((null err)                                   ; clean load
          (setq scheme-ts--last-confirmed-error nil)
          (message "%s done" label))
         ((equal err scheme-ts--last-confirmed-error)  ; known genuine: no thrash
          (geiser-debug--display-retort label ret))
         ((string-match-p scheme-ts--stale-error-rx err)
          ;; Defer out of the process filter, then restart + reload clean.
          (run-at-time
           0 nil
           (lambda ()
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (message "Scheme: REPL state may be stale; restarting Chez and reloading...")
                 (geiser-repl-restart-repl)
                 (scheme-ts--reload-after-restart buf label))))))
         (t                                            ; ordinary error
          (geiser-debug--display-retort label ret)))))))

(defun scheme-ts--reload-after-restart (buf label)
  "Reload BUF after a REPL restart; a surviving error is genuine, so cache it."
  (when (and (buffer-live-p buf) (geiser-repl--connection*))
    (with-current-buffer buf
      (geiser-autodoc--clean-cache)
      (let* ((ret (geiser-eval--send/wait (scheme-ts--load-file-code)))
             (err (scheme-ts--retort-error-text ret)))
        (if (null err)
            (progn (setq scheme-ts--last-confirmed-error nil)
                   (message "%s done (after clean restart)" label))
          ;; Survived a fresh REPL => real error; remember it so the next save
          ;; with the same error skips the restart.
          (setq scheme-ts--last-confirmed-error err)
          (geiser-debug--display-retort label ret)
          (message "Scheme: error persists after a clean reload -- real error, not staleness."))))))

(defun scheme-ts-ensure-repl ()
  "Start a Chez Geiser REPL if one isn't already running.
Starts it in the background so the current window layout is preserved.
Return non-nil when a new REPL was started."
  (unless (seq-some (lambda (b)
                      (and (buffer-live-p b)
                           (get-buffer-process b)
                           (with-current-buffer b
                             (eq geiser-impl--implementation 'chez))))
                    geiser-repl--repls)
    (save-window-excursion (geiser 'chez))
    t))

(defun scheme-ts-find-definition ()
  "Jump to definition, preferring Geiser, then falling back to `xref'.
Geiser resolves symbols the running REPL knows (your loaded code); xref
covers whatever a project makes available to it (e.g. an etags index for
built-in / library procedures the REPL keeps no source for)."
  (interactive)
  (let ((start (point)) (start-buf (current-buffer)))
    (condition-case nil (geiser-edit-symbol-at-point) (error nil))
    (when (and (eq (current-buffer) start-buf) (= (point) start))
      (let ((sym (thing-at-point 'symbol t)))
        (when sym (ignore-errors (xref-find-definitions sym)))))))

(defun scheme-ts--load-when-ready (buf &optional tries)
  "Load BUF into the Chez REPL once a connection exists, retrying until it does.
Geiser evaluates a buffer's forms in that buffer's library module (here
`(sqlite)' &c.), so until the file is loaded into the REPL every eval fails with
\"library ... is not loaded\" -- the procedures appear \"undefined\".  The old
open-time load fired exactly once and was simply skipped when the freshly
cold-started REPL's connection wasn't live yet, so the file stayed unloaded
until the first save.  We instead poll for the connection (~0.3s x 50 = 15s cap)
and load as soon as it is up.  (We gate on the *connection*, not a probe eval:
a probe would itself run in the not-yet-loaded module and never succeed.)"
  (let ((tries (or tries 0)))
    (when (and (buffer-live-p buf) (buffer-file-name buf))
      (with-current-buffer buf
        (cond
         ((geiser-repl--connection*)
          (ignore-errors (scheme-ts-auto-load-on-save buf)))
         ((< tries 50)
          (run-at-time 0.3 nil #'scheme-ts--load-when-ready buf (1+ tries)))
         (t
          (message "scheme-ts: Chez REPL never connected; %s not auto-loaded"
                   (buffer-name buf))))))))

(defun scheme-ts-setup-geiser ()
  "Activate geiser, ensure a REPL, and load the buffer for introspection.
Loading the library into the REPL is what makes jump-to-definition,
autodoc and completion work; without it geiser cannot resolve symbols.

Geiser activation and REPL startup are wrapped in `with-demoted-errors':
they spawn an external Chez process, and an error here would propagate out
of `scheme-mode-hook' and abort `run-mode-hooks' *before*
`after-change-major-mode-hook' -- the hook that enables font-lock.  A
failed REPL start would then silently leave the buffer with no syntax
highlighting (and no keybindings).  Demoting keeps the mode hook intact."
  (with-demoted-errors "scheme-ts: geiser activation failed: %S"
    (turn-on-geiser-mode))
  (add-hook 'after-save-hook #'scheme-ts-auto-load-on-save nil t)
  ;; M-.: Geiser first, then xref (whatever the project wires up) as fallback.
  ;; Override only M-. while inheriting the rest of `geiser-mode-map'.
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map geiser-mode-map)
    (define-key map (kbd "M-.") #'scheme-ts-find-definition)
    (setq-local minor-mode-overriding-map-alist
                (cons (cons 'geiser-mode map) minor-mode-overriding-map-alist)))
  ;; evil `gd': evil-collection binds it straight to `geiser-edit-symbol-at-point';
  ;; a buffer-local binding wins over that minor-mode map and adds the fallback.
  (when (fboundp 'evil-local-set-key)
    (evil-local-set-key 'normal (kbd "gd") #'scheme-ts-find-definition))
  (when buffer-file-name
    (with-demoted-errors "scheme-ts: REPL startup failed: %S"
      (scheme-ts-ensure-repl)
      ;; Load this buffer into the REPL so M-. & friends resolve immediately.
      ;; Deferred + retried rather than fired once inline: a just-cold-started
      ;; REPL isn't reliably ready to eval the instant `geiser' returns, which
      ;; used to leave the file unloaded until the first save (see
      ;; `scheme-ts--load-when-ready').
      (scheme-ts--load-when-ready (current-buffer)))))

;; General Scheme editing uses the built-in `scheme-mode'.  Benchmarks showed
;; `scheme-ts-mode' is ~3-4x slower at full fontification and ~2x slower per
;; keystroke even at its lowest correct font-lock level: tree-sitter's
;; per-node query machinery is pure overhead on Scheme's trivial lexical
;; structure, where regex font-lock is already near-optimal.
;;
;; `scheme-ts-mode' is kept ONLY to fontify scheme code blocks inside markdown
;; (see `markdown-ts-code-block-source-mode-map' below).  That path is
;; mandatory: `markdown-ts-mode' harvests `treesit-font-lock-settings' from the
;; mapped mode and fontifies blocks with an embedded tree-sitter parser, so a
;; regex mode like `scheme-mode' supplies nothing there.  The geiser/REPL
;; helpers above are mode-agnostic, so they run on `scheme-mode' instead.
(use-package scheme
  :ensure nil
  :mode (("\\.scm\\'" . scheme-mode)
         ("\\.sld\\'" . scheme-mode)
         ("\\.sls\\'" . scheme-mode))
  :hook (scheme-mode . scheme-ts-setup-geiser))

(use-package scheme-ts-mode
  :ensure nil
  :load-path "lisp/"
  :demand t
  :config
  (with-eval-after-load 'markdown-ts-mode
    (add-to-list 'markdown-ts-code-block-source-mode-map
                 '(scheme . scheme-ts-mode))))

;; Structural editing for Lisp/Scheme: the full paredit editing surface from
;; evil NORMAL state on a `,' localleader, paren-safe evil operators (without
;; rebinding them), and continuous automatic indentation.  Replaces symex.
;;
;;   paredit                -> the editing command library (+ insert auto-pairing)
;;   enhanced-evil-paredit  -> makes d/c/x/p paren-safe WITHOUT rebinding them
;;   aggressive-indent      -> keeps the enclosing form correctly indented
;;   general                -> the `,' localleader, scoped to paredit buffers

;; The `,' localleader is scoped to `paredit-mode-map', so the only key it
;; shadows is evil's `,' (repeat-find-char-reverse), and only inside lisp
;; buffers -- everywhere else reverse-find-repeat is untouched.  It lives in
;; paredit's own `:config' (which runs after paredit loads, so
;; `paredit-mode-map' exists); `general' is already loaded eagerly (`:demand t'
;; at the top of the file), so we must NOT re-declare it with `use-package
;; general' here -- that re-queues the package in elpaca ("Duplicate item ID
;; queued: general") and aborts init.
(use-package paredit
  :hook ((emacs-lisp-mode
          lisp-mode
          lisp-interaction-mode
          scheme-mode
          eval-expression-minibuffer-setup) . enable-paredit-mode)
  :config
  (general-create-definer my/lisp-localleader
    :states  '(normal visual)
    :keymaps 'paredit-mode-map
    :prefix  ",")

  (my/lisp-localleader
    "" '(:ignore t :which-key "lisp")

    ;; -- slurp / barf --------------------------------------------------
    "s" '(paredit-forward-slurp-sexp   :which-key "slurp ->")
    "b" '(paredit-forward-barf-sexp    :which-key "barf ->")
    "S" '(paredit-backward-slurp-sexp  :which-key "slurp <-")
    "B" '(paredit-backward-barf-sexp   :which-key "barf <-")

    ;; -- splice / raise / convolute ------------------------------------
    "x" '(paredit-splice-sexp                  :which-key "splice")
    "<" '(paredit-splice-sexp-killing-backward :which-key "splice kill <-")
    ">" '(paredit-splice-sexp-killing-forward  :which-key "splice kill ->")
    "r" '(paredit-raise-sexp                   :which-key "raise")
    "v" '(paredit-convolute-sexp               :which-key "convolute")

    ;; -- wrap (works on the visual selection too) ----------------------
    "(" '(paredit-wrap-round        :which-key "wrap ()")
    "[" '(paredit-wrap-square       :which-key "wrap []")
    "{" '(paredit-wrap-curly        :which-key "wrap {}")
    "\"" '(paredit-meta-doublequote :which-key "wrap \"\"")

    ;; -- split / join / transpose --------------------------------------
    "/" '(paredit-split-sexp :which-key "split")
    "j" '(paredit-join-sexps :which-key "join")
    "t" '(transpose-sexps    :which-key "transpose")

    ;; -- kill / comment ------------------------------------------------
    "k" '(paredit-kill         :which-key "kill to eol")
    ";" '(paredit-comment-dwim :which-key "comment dwim")

    ;; -- navigation ----------------------------------------------------
    "f" '(paredit-forward      :which-key "fwd sexp")
    "h" '(paredit-backward     :which-key "back sexp")
    "u" '(paredit-backward-up  :which-key "up (out)")
    "n" '(paredit-forward-down :which-key "down (in)")

    ;; -- explicit reindent ---------------------------------------------
    "=" '(paredit-reindent-defun :which-key "indent defun")))

;; enhanced-evil-paredit makes d / c / x / p / y paren-safe -- but it does so by
;; *rebinding* them in `enhanced-evil-paredit-mode-map' to its own operators
;; (e.g. `c' -> `enhanced-evil-paredit-change').  That minor-mode map shadows the
;; global `cc' -> `cw' dispatch defined on `c' up in the general config, so in
;; lisp/scheme buffers `cc' would otherwise fall back to change-whole-line.
;; Re-apply the same dispatch here on top of the paren-safe change operator: `cc'
;; = change-word, while `c<motion>' stays balanced.
(use-package enhanced-evil-paredit
  :hook (paredit-mode . enhanced-evil-paredit-mode)
  :config
  (general-define-key
   :states  'normal
   :keymaps 'enhanced-evil-paredit-mode-map
   "c" (general-key-dispatch 'enhanced-evil-paredit-change
         "c" (general-simulate-key ('enhanced-evil-paredit-change "e")))))

;; Reindents the enclosing defun after every edit.  Safe here because paredit
;; guarantees the structure stays balanced.
(use-package aggressive-indent
  :hook ((emacs-lisp-mode
          lisp-mode
          lisp-interaction-mode
          scheme-mode
          racket-mode) . aggressive-indent-mode))

(use-package rainbow-delimiters)

(use-package paren-face
  :hook ((racket-mode emacs-lisp-mode) . paren-face-mode)
  )


;; ==============================================================================
;; Frontend
;; ==============================================================================

(use-package treesit-auto
  :ensure t
  :config
  (setq treesit-auto-install t)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package vue-ts-mode
  :ensure (:host github :repo "8uff3r/vue-ts-mode"))



;; ==============================================================================
;; Vterm
;; ==============================================================================

(use-package vterm
  :config

  (add-hook 'vterm-mode-hook (lambda ()
  			       (setq-local global-hl-line-mode nil)))

  (setq vterm-max-scrollback 50000)

  )

;; ==============================================================================
;; Eat
;; ==============================================================================

(use-package eat
  :ensure (
	   :type git
	   :host codeberg
	   :repo "akib/emacs-eat"
	   :files ("*.el" ("term" "term/*.el") "*.texi"
		   "*.ti" ("terminfo/e" "terminfo/e/*")
		   ("terminfo/65" "terminfo/65/*")
		   ("integration" "integration/*")
		   (:exclude ".dir-locals.el" "*-tests.el"))
	   :branch "master")
					;:hook
					;(eat-exec . (lambda (&rest _) (eat-line-mode) (evil-insert 1)))

  :init
  (pretty-hydra-define term-hydra
    (:color blue :quit-key "q" :title "Terminal")
    ("Terminal"
     (( "t" (eat-persp #'pop-to-buffer) "Raise terminal")
      ( "T" (eat-persp) "Raise terminal same window")
      )
     "Commands"
     (( "p" (eat-run-previous-command) "re-run previous command"))
     ))

  :config

  (defun eat-line-send-history-search ()
    "Send `C-r' to the shell."
    (interactive)
					;(delete-region (eat-term-end eat-terminal) (point-max))
    (goto-char (point-max))
    (insert "\C-r")
    (eat-line-send))


  (evil-define-key '(normal insert) eat-line-mode-map (kbd "C-r") #'consult-atuin)
  (evil-define-key 'normal eat-semi-char-mode-map (kbd "RET") (lambda () (interactive)(goto-char (point-max)) (eat-line-send)))
  (evil-define-key 'normal eat-line-mode-map (kbd "RET") (lambda () (interactive) (eat-line-send-input)))

  (defun persp-eat-name ()
    (format "%s<%s>" eat-buffer-name (persp-current-name)))

  (defun eat-persp (&optional maybe-display-func)
    (interactive)
    "Start a new Eat terminal emulator in a buffer.

PROGRAM and ARG is same as in `eat' and `eat-other-window'.
DISPLAY-BUFFER-FN is the function to display the buffer."
    (let ((program (funcall eat-default-shell-function))
	  (display-func (or maybe-display-func #'pop-to-buffer-same-window))
	  (buffer(get-buffer-create (persp-eat-name))))
      (with-current-buffer buffer
	(unless (eq major-mode #'eat-mode)
          (eat-mode))
	(funcall display-func buffer)
	(unless (and eat-terminal
                     (eat-term-parameter eat-terminal 'eat--process))
          (eat-exec buffer (buffer-name) "/usr/bin/env" nil
                    (list "sh" "-c" program)))
	buffer)))


  (defun eat-persp-display-latest ()
    (interactive)
    (let ((buf (get-buffer (persp-eat-name))))
      (display-buffer buf)
      (with-selected-window (get-buffer-window buf)
	(goto-char (point-max)))))

  (defun run-in-persp-eat (fun)
    (let* ((buf (get-buffer (persp-eat-name)))
	   (win (get-buffer-window buf)))
      (if (window-live-p win)
	  (with-selected-window win
	    (funcall fun)) 
	(with-current-buffer buf
	  (funcall fun)))))

  (defun eat-run-previous-command ()
    "Send `!!' to the shell."
    (interactive)
    (run-in-persp-eat (lambda ()
			(delete-region (eat-term-end eat-terminal) (point-max))
			(goto-char (point-max))
			(eat-term-send-string eat-terminal "!!\n"))))

  (defun persp-eat-bell (_) (eat-persp-display-latest))
  (advice-add 'eat--bell :override 'persp-eat-bell)


  )

;; ==============================================================================
;; C(++)
;; ==============================================================================

(use-package cmake-mode
  :mode "CMakeLists.txt")


;; ==============================================================================
;; Text / Comments
;; ==============================================================================

(use-package filladapt
  :ensure t
  :init
  (setq-default filladapt-mode t)
  :config
  (add-hook 'c-mode-common-hook
	    (lambda ()
	      (when (featurep 'filladapt)
		(c-setup-filladapt)))) )

;; ==============================================================================
;; Scala
;; ==============================================================================

(defun insert-unix-timestamp ()
  (interactive)
  (insert (replace-regexp-in-string "\n\\'" "" (shell-command-to-string "date +%s"))))

(defun insert-uuid ()
  (interactive)
  (insert (replace-regexp-in-string "\n\\'" "" (shell-command-to-string "cat /proc/sys/kernel/random/uuid"))))

(defun insert-ulid ()
  (interactive)
  (insert (replace-regexp-in-string "\n\\'" "" (shell-command-to-string "ulid"))))

;; Enable scala-mode and sbt-mode
(use-package scala-mode
  ;;:hook (scala-mode . scalafmt-on-save-mode)
  :interpreter ("scala" . scala-mode)
  :config
  (add-hook 'scala-mode-hook
    (lambda () (add-hook 'before-save-hook 'eglot-format nil t)))
  (evil-define-key 'normal scala-mode-map (kbd ",a") 'eglot-code-actions)
  (defun my-eglot-organize-imports ()
					;(interactive)
    (eglot-code-actions nil nil "source.organizeImports" t))
  ;there seems to be a fucky race condition with this and eglot format
					;(add-hook 'before-save-hook 'my-eglot-organize-imports)
  )


;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))


;; Enable nice rendering of diagnostics like compile errors.
(use-package flycheck
  :hook (scala-mode . flycheck-mode))

(use-package lsp-mode
  ;; Optional - enable lsp-mode automatically in scala files
  ;; You could also swap out lsp for lsp-deffered in order to defer loading
  ;; :hook (scala-mode . lsp)
					;(lsp-mode . lsp-lens-mode)
  :config
  ;; Uncomment following section if you would like to tune lsp-mode performance according to
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq gc-cons-threshold 100000000) ;; 100mb
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-idle-delay 0.500)
  (setq lsp-log-io nil)
  (setq lsp-completion-provider :capf)
  (setq lsp-prefer-flymake nil)
  ;; Makes LSP shutdown the metals server when all buffers in the project are closed.
  ;; https://emacs-lsp.github.io/lsp-mode/page/settings/mode/#lsp-keep-workspace-alive
  (setq lsp-keep-workspace-alive nil))

;; Add metals backend for lsp-mode
(use-package lsp-metals)

;; ==============================================================================
;; SQL
;; ==============================================================================

(use-package sql-indent
  :ensure (:host github :repo "alex-hhh/emacs-sql-indent")
  :hook ((sql-mode . sqlind-minor-mode)))

(use-package sql-mode
  :ensure nil
  :hook ((sql-mode . pgformatter-on-save-mode)))

(use-package js-ts-mode
  :ensure nil
  :hook ((js-ts-mode . prettier-on-save-mode)))

(use-package typescript-ts-mode
  :ensure nil
  :hook ((typescript-ts-mode . prettier-on-save-mode)))



;; ==============================================================================
;; YAML
;; ==============================================================================

(use-package yaml-mode
  )

(use-package jsonnet-mode
  )

;; ==============================================================================
;; PHP
;; ==============================================================================

(use-package php-mode
  )

;; ==============================================================================
;; GUI
;; ==============================================================================

(use-package popper
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
	  "*Embark Actions*"
	  "Errors List"
	  "*eldoc*"
          help-mode
          compilation-mode
	  flymake-diagnostics-buffer-mode
	  flymake-project-diagnostics-mode))
  (popper-mode +1)
  (popper-echo-mode +1); For echo area hints
  )

;; ==============================================================================
;; Dired
;; ==============================================================================

(use-package dired
  :ensure nil
  :custom
  ;; Human-readable sizes, directories grouped at top, classify file types with suffixes.
  (dired-listing-switches "-alFh --group-directories-first")
  ;; Don't accumulate dired buffers when navigating directories.
  (dired-kill-when-opening-new-dired-buffer t)
  ;; With two dired windows, copy/rename targets the other directory.
  (dired-dwim-target t)
  ;; Allow recursive copy/delete without asking each time.
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  ;; Offer to create parent directories when copying/moving.
  (dired-create-destination-dirs 'ask)
  ;; Auto-refresh dired buffers when revisiting.
  (dired-auto-revert-buffer t)
  :hook
  ;; Hide file details (permissions, owner, size) by default for a cleaner view.
  ;; Toggle with "(" to see full details.
  (dired-mode . dired-hide-details-mode))

;; ==============================================================================
;; Window management
;; ==============================================================================


(setq paragraph-start "\f\\|\\s*-\\|[ \t]*$")


(defun my-change-window-divider ()
  (let ((display-table (or buffer-display-table standard-display-table)))
    (set-display-table-slot display-table 5 ?│)
    (set-window-display-table (selected-window) display-table)))

(add-hook 'window-configuration-change-hook 'my-change-window-divider)

(defun scroll-by-percent (percent)
  "Scroll by PERCENT of the window height.
Positive values scroll down, negative values scroll up."
  (interactive "nScroll by percent: ")
  (let ((lines (ceiling (* (/ percent 100.0) (window-height)))))
    (scroll-up lines)))

(defun scroll-down-25-percent ()
  "Scroll down by 25% of the window height."
  (interactive)
  (scroll-by-percent 25))

(general-nmap "C-y" (lambda () (interactive) (scroll-by-percent -20)))
(general-nmap "C-e" (lambda () (interactive) (scroll-by-percent 20)))

(use-package claude-code-ide
  :ensure (:host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  :config
  (claude-code-ide-emacs-tools-setup)
  (setq claude-code-ide--cli-available t)
  (setq claude-code-ide-terminal-backend 'eat)
  (setq claude-code-ide-cli-path "claude"))

(use-package claudemacs
  :ensure (:host github :repo "cpoile/claudemacs")
  :config
  (defun my/claudemacs-handle-window-resize (frame)
    "Propagate window size changes to eat terminals in claudemacs buffers.

Claudemacs sets `window-adjust-process-window-size-function' to `ignore'
once the buffer content exceeds the window height. This prevents an
annoying scroll-reset-on-buffer-switch issue (similar to vterm #149),
but it also means that eat never learns about *real* window resizes,
so the terminal stays stuck at its original dimensions.

This function is added to `window-size-change-functions' and runs
whenever any window in FRAME changes size. For each claudemacs buffer
whose auto-adjust has been disabled, we manually resize eat's internal
terminal and then update the PTY dimensions (sending SIGWINCH to the
subprocess) so Claude Code redraws at the correct size."
    (dolist (window (window-list frame))
      (with-current-buffer (window-buffer window)
        (when (and (claudemacs--is-claudemacs-buffer-p)
                   (boundp 'eat-terminal) eat-terminal
                   (eq window-adjust-process-window-size-function 'ignore))
          (let* ((process (eat-term-parameter eat-terminal 'eat--process))
                 (size (when (and process (process-live-p process))
                         (window-adjust-process-window-size-smallest
                          process (list window)))))
            (when size
              (let* ((width  (max (car size) 1))
                     (height (max (cdr size) 1))
                     (term-size (eat-term-size eat-terminal)))
                (unless (and (= width  (car term-size))
                             (= height (cdr term-size)))
                  (let ((inhibit-read-only t))
                    (eat-term-resize eat-terminal width height)
                    (eat-term-redisplay eat-terminal)
                    (set-process-window-size process height width)
                    (set-window-start window (eat-term-display-beginning eat-terminal) t)
                    (set-window-point window (eat-term-display-cursor eat-terminal)))))))))))
  (add-hook 'window-size-change-functions #'my/claudemacs-handle-window-resize))

(setq split-height-threshold nil)

(setq fill-column 80)

;; Auto-run hpack when saving package.yaml
(defun auto-hpack ()
  "Run hpack if package.yaml is saved and a .cabal file exists in the same directory."
  (when (and (string-equal (file-name-nondirectory buffer-file-name) "package.yaml")
             (directory-files default-directory nil "\\.cabal$"))
    (let ((default-directory (file-name-directory buffer-file-name)))
      (shell-command "hpack"))))

(add-hook 'yaml-ts-mode-hook 
          (lambda () (add-hook 'after-save-hook 'auto-hpack nil t)))

(use-package emacs-mcp
  :demand t
  :ensure (emacs-mcp :host github :repo "mpontus/emacs-mcp"))

(use-package eca
  :ensure (eca :host github :repo "editor-code-assistant/eca-emacs"))

(use-package zoom
  :config
  (defun size-callback ()
    (cond ((eq major-mode 'scala-mode) '(120 . 0.5))
          (t                            '(nil . nil))))

  (setq zoom-size 'size-callback)

  (setq zoom-mode t)

  )
