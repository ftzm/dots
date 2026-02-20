;; -*- lexical-binding: t; -*-

;; ---------------------------------------------------------------------------- 
;; Elpaca

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
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
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

(set-frame-font "Iosevka ftzm Medium 18")
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
  )

;; ----------------------------------------------------------------------------

(use-package doom-themes
  :config
  (load-theme 'doom-gruvbox t)
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

(use-package undo-fu)

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

(use-package meow
  :demand t
  :init
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-dvorak)
    (meow-leader-define-key
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))
    (meow-motion-define-key
     ;; custom keybinding for motion state
     '("<escape>" . ignore))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("<" . meow-beginning-of-thing)
     '(">" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-line)
     '("E" . meow-goto-line)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-join)
     '("k" . meow-kill)
     '("l" . meow-till)
     '("m" . meow-mark-word)
     '("M" . meow-mark-symbol)
     '("n" . meow-next)
     '("N" . meow-next-expand)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-prev)
     '("P" . meow-prev-expand)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-search)
     '("t" . meow-right)
     '("T" . meow-right-expand)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-next-word)
     '("W" . meow-next-symbol)
     '("x" . meow-save)
     '("X" . meow-sync-grab)
     '("y" . meow-yank)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore)))
  :config
  ;; this doesn't work with subword mode
  (defun forward-vimlike-word (&optional arg)
    "Alternate `forward-word'. Essentially the same idea as Vim's 'e'."
    (interactive "^p")
    (setq arg (or arg 1))
    (cl-destructuring-bind (sign move-func char-func)
	(if (>= arg 0)
            '(1 skip-syntax-forward char-after)
          '(-1 skip-syntax-backward char-before))
      (with-syntax-table (standard-syntax-table)
	(let ((distance sign))
          (while (and distance (> (abs distance) 0) (> (* arg sign) 0))
            (setq distance
                  (when-let ((next-char (funcall char-func))
                             (next-syntax (char-syntax next-char)))
                    (cond ((eq next-syntax ?w)
                           (funcall move-func "w"))
                          ((eq next-syntax ?\ )
                           (prog1
                               (funcall move-func " ")
                             (forward-vimlike-word sign)))
                          (t
                           (funcall move-func "^w ")))))
            (setq arg (- arg sign)))
          (and distance (> (abs distance) 0))))))

					;(put 'vimlike-word 'forward-op #'forward-vimlike-word)

					;(setq meow-word-thing 'vimlike-word) 

  ;; (meow-setup)
  ;; (meow-global-mode 1)
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
  (general-nmap "s" 'avy-goto-char-flex)


  (general-create-definer my-leader-def
    :prefix "SPC")

  (my-leader-def
    :states '(normal visual motion)
    :keymaps 'override
    "SPC" '(execute-extended-command :which-key "command")
    ;; the default behavior of `other buffer' is to ignore buffers
    ;; open in other windows, which can be annoying when you have two
    ;; views of the same buffer going. This invocation considers all
    ;; buffers except for the current one.
    "'" '((lambda () (interactive) (switch-to-buffer (other-buffer (current-buffer) t))) :which-key "other buffer")
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
   "b" 'switch-to-buffer
   "B" 'switch-to-buffer-other-window
   "s" 'save-buffer
   "f" 'find-file
   "w" 'write-file
					;"r" 'consult-recent-file
					;"u" 'sudo-find-file
					;"U" 'sudo-this-file
   )

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

;; Automatic tablist marking on visual selection
(use-package tablist)

(use-package evil-tablist-visual-mark
  :ensure nil
  :load-path "~/.config/emacs/lisp/"
  :after (evil tablist)
  :config
  (evil-tablist-visual-mark-mode 1))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode)
  (setq which-key-idle-delay 0.3))


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
	doom-modeline-icon nil
	doom-modeline-position-column-line-format '("%l:%c")
	doom-modeline-buffer-encoding nil
					;doom-modeline-percent-position nil
	)
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
    (orderless-smart-case nil)
    )
  )

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
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
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
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

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
  :load-path "~/.config/emacs/lisp/"
  :after consult)

(use-package embark
  :demand t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
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
					;(global-corfu-mode)
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
  )

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

;; ==============================================================================
;; Ligatures
;; ==============================================================================

;; (use-package composite
;;   :ensure nil
;;   :init
;;   (defvar composition-ligature-table (make-char-table nil))
;;   :hook
;;   (((prog-mode conf-mode nxml-mode markdown-mode help-mode)
;;     . (lambda () (setq-local composition-function-table composition-ligature-table))))
;;   :config
;;   ;; support ligatures, some toned down to prevent hang
;;   (when (version<= "27.0" emacs-version)
;;     (let ((alist
;;            '((33 . ".\\(?:\\(==\\|[!=]\\)[!=]?\\)")
;;              (35 . ".\\(?:\\(###?\\|_(\\|[(:=?[_{]\\)[#(:=?[_{]?\\)")
;;              (36 . ".\\(?:\\(>\\)>?\\)")
;;              (37 . ".\\(?:\\(%\\)%?\\)")
;;              (38 . ".\\(?:\\(&\\)&?\\)")
;;              (42 . ".\\(?:\\(\\*\\*\\|[*>]\\)[*>]?\\)")
;;              ;; (42 . ".\\(?:\\(\\*\\*\\|[*/>]\\).?\\)")
;;              (43 . ".\\(?:\\([>]\\)>?\\)")
;;              ;; (43 . ".\\(?:\\(\\+\\+\\|[+>]\\).?\\)")
;;              (45 . ".\\(?:\\(-[->]\\|<<\\|>>\\|[-<>|~]\\)[-<>|~]?\\)")
;;              ;; (46 . ".\\(?:\\(\\.[.<]\\|[-.=]\\)[-.<=]?\\)")
;;              (46 . ".\\(?:\\(\\.<\\|[-=]\\)[-<=]?\\)")
;;              (47 . ".\\(?:\\(//\\|==\\|[=>]\\)[/=>]?\\)")
;;              ;; (47 . ".\\(?:\\(//\\|==\\|[*/=>]\\).?\\)")
;;              (48 . ".\\(?:\\(x[a-fA-F0-9]\\).?\\)")
;;              (58 . ".\\(?:\\(::\\|[:<=>]\\)[:<=>]?\\)")
;;              (59 . ".\\(?:\\(;\\);?\\)")
;;              (60 . ".\\(?:\\(!--\\|\\$>\\|\\*>\\|\\+>\\|-[-<>|]\\|/>\\|<[-<=]\\|=[<>|]\\|==>?\\||>\\||||?\\|~[>~]\\|[$*+/:<=>|~-]\\)[$*+/:<=>|~-]?\\)")
;;              (61 . ".\\(?:\\(!=\\|/=\\|:=\\|<<\\|=[=>]\\|>>\\|[=>]\\)[=<>]?\\)")
;;              (62 . ".\\(?:\\(->\\|=>\\|>[-=>]\\|[-:=>]\\)[-:=>]?\\)")
;;              (63 . ".\\(?:\\([.:=?]\\)[.:=?]?\\)")
;;              (91 . ".\\(?:\\(|\\)[]|]?\\)")
;;              ;; (92 . ".\\(?:\\([\\n]\\)[\\]?\\)")
;;              (94 . ".\\(?:\\(=\\)=?\\)")
;;              (95 . ".\\(?:\\(|_\\|[_]\\)_?\\)")
;;              (119 . ".\\(?:\\(ww\\)w?\\)")
;;              (123 . ".\\(?:\\(|\\)[|}]?\\)")
;;              (124 . ".\\(?:\\(->\\|=>\\||[-=>]\\||||*>\\|[]=>|}-]\\).?\\)")
;;              (126 . ".\\(?:\\(~>\\|[-=>@~]\\)[-=>@~]?\\)"))))
;;       (dolist (char-regexp alist)
;;         (set-char-table-range composition-ligature-table (car char-regexp)
;;                               `([,(cdr char-regexp) 0 font-shape-gstring]))))
;;     (set-char-table-parent composition-ligature-table composition-function-table))
;;   )

;; ==============================================================================
;; Git
;; ==============================================================================

(use-package transient)

(use-package magit
  :commands (magit-status magit-keys)
  :custom (magit-bury-buffer-function #'magit-restore-window-configuration)
  :config
  (define-key transient-map        (kbd "<escape>") 'transient-quit-one)
  (define-key transient-edit-map   (kbd "<escape>") 'transient-quit-one)
  (define-key transient-sticky-map (kbd "<escape>") 'transient-quit-seq)

  (general-define-key
   :prefix-command 'magit-keys
   "s" 'magit-status
   "b" 'magit-blame
   "B" 'magit-blame-quit)

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
  )

(use-package git-link
  :commands git-link)

(use-package emacs
  :ensure nil
  :after general
  :init
  (general-define-key
   :prefix-command 'magit-keys
   "s" 'magit-status
   "b" 'magit-blame
   "B" 'magit-blame-quit)
  )


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
    (when (eq char1 ?)
      (setq char1 ?\n))
    (if (not (eq char2 ?))
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
   "g" 'consult-git-grep
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

  (add-to-list 'eglot-server-programs
               `(vue-ts-mode . ("vue-language-server" "--stdio" :initializationOptions ,(vue-eglot-init-options))))

  (add-to-list 'eglot-server-programs '(nix-ts-mode . ("nil" :initializationOptions (:formatting (:command ["alejandra"])))))

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

(use-package aggressive-indent
  :hook (elisp-mode . aggressive-indent-mode))

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
  (add-hook 'before-save-hook 'eglot-format)
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
    :key-form (org-super-agenda--when-with-marker-buffer (org-super-agenda--get-marker item)
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

(use-package paren-face
  :hook ((racket-mode emacs-lisp-mode) . paren-face-mode)
  )

(use-package aggressive-indent
  :ensure nil
  :hook ((racket-mode emacs-lisp-mode) . aggressive-indent-mode)
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
;; Gerbil
;; ==============================================================================

(use-package rainbow-delimiters
  )

(use-package gambit
  :ensure (
	   :host github
	   :repo "gambit/gambit"
	   :files ("misc/gambit.el")
	   :branch "master"))

(use-package gerbil-mode
  :ensure (
	   :host github
	   :repo "mighty-gerbils/gerbil"
	   :files ("etc/gerbil-mode.el")
	   :branch "master")
  :defer t
  :mode (("\\.ss\\'"  . gerbil-mode)
	 ("\\.pkg\\'" . gerbil-mode))
  :bind (:map comint-mode-map
	      (("C-S-n" . comint-next-input)
	       ("C-S-p" . comint-previous-input)
	       ("C-S-l" . clear-comint-buffer))
	      :map gerbil-mode-map
	      (("C-S-l" . clear-comint-buffer)))
  :init
					; (autoload 'gerbil-mode
					;   (expand-file-name "share/emacs/site-lisp/gerbil-mode.el" *gerbil-path*)
					;   "Gerbil editing mode." t)
  :hook
  ((gerbil-mode-hook . linum-mode)
   (gerbil-mode-hook . rainbow-delimiters-mode)
   (inferior-scheme-mode-hook . gambit-inferior-mode))
  :config
  (require 'gambit)
					;(setf scheme-program-name (expand-file-name "bin/gxi" *gerbil-path*))
  (setf scheme-program-name "gxi")

  ;; (let ((tags (locate-dominating-file default-directory "TAGS")))
  ;;   (when tags (visit-tags-table tags)))
  ;; (let ((tags (expand-file-name "src/TAGS" *gerbil-path*)))
  ;;   (when (file-exists-p tags) (visit-tags-table tags)))

  (when (package-installed-p 'smartparens)
    (sp-pair "'" nil :actions :rem)
    (sp-pair "`" nil :actions :rem))

  (defun clear-comint-buffer ()
    (interactive)
    (with-current-buffer "*scheme*"
      (let ((comint-buffer-maximum-size 0))
	(comint-truncate-buffer))))

  (defun gerbil-setup-buffers ()
    "Change current buffer mode to gerbil-mode and start a REPL"
    (interactive)
    (gerbil-mode)
    (split-window-right)
    (shrink-window-horizontally 2)
    (let ((buf (buffer-name)))
      (other-window 1)
      (run-scheme "gxi")
      (switch-to-buffer-other-window "*scheme*" nil)
      (switch-to-buffer buf)))

  (global-set-key (kbd "C-c C-g") 'gerbil-setup-buffers))

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
  (add-hook 'before-save-hook 'eglot-format)
  (evil-define-key 'normal scala-mode-map (kbd ",a") 'eglot-code-actions)
  (defun my-eglot-organize-imports ()
    (interactive)
    (eglot-code-actions nil nil "source.organizeImports" t))
  (add-hook 'before-save-hook 'my-eglot-organize-imports)
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
  :config
  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq dired-listing-switches "-alFh")
  )


;; ==============================================================================
;; Rest client
;; ==============================================================================


					;(use-package restclient
					;:ensure (
					;:host github
					;:repo "casouri/restclient.el"
					;:files ( "gql-builder.el" "restclient-jq.el" "restclient.el" )
					;:branch "master"))

;; ==============================================================================
;; Window management
;; ==============================================================================

;; (use-package shackle
;;   :config
;;   (setq popper-display-control nil)
;;   ;; (setq split-width-threshold 1)
;;   (setq shackle-default-rule '(:select t))
;;   (setq shackle-rules
;; 	'((help-mode :select t :align right)
;;           (compilation-mode :select t :size 0.3 :align below)))
;;   (shackle-mode 1))

(setq paragraph-start "\f\\|\\s*-\\|[ \t]*$")


(defun my-change-window-divider ()
  (let ((display-table (or buffer-display-table standard-display-table)))
    (set-display-table-slot display-table 5 ?│)
    (set-window-display-table (selected-window) display-table)))

(add-hook 'window-configuration-change-hook 'my-change-window-divider)

(set-face-background 'vertical-border nil)
(set-face-foreground 'vertical-border (face-foreground 'mode-line-inactive))

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
  (setq claude-code-ide-cli-path "npx claude --"))

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

;; ------------------------------------------------
;; acp shell thingie

(use-package shell-maker
  :ensure (:host github :repo "xenodium/shell-maker.el"))

(use-package acp
  :ensure (:host github :repo "xenodium/acp.el"))

(use-package agent-shell
  :ensure (:host github :repo "xenodium/agent-shell")
  :config
  (require 'acp)
  (require 'shell-maker)
  (setq agent-shell-anthropic-claude-command '("npx" "claude-code-acp"))
  )

