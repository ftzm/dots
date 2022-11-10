(setq scroll-step 1
      scroll-conservatively 10000) ;; scroll by 1 like in vim

(use-package evil
  :straight t
  :init
  ;; for evil-collections
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-redo)
  ;; for visual line mode
  (setq evil-respect-visual-line-mode t)

  (evil-mode t)

  (defun ftzm/format-evil-tags ()
    (let ((theme-bg (face-attribute 'default :background))
	  (theme-highlight (face-attribute 'highlight :background))
	  (theme-color-1 (face-attribute 'font-lock-function-name-face :foreground))
	  (theme-color-2 (face-attribute 'font-lock-string-face :foreground)))
      (setq evil-normal-state-tag   (propertize " N " 'face (list :background theme-color-1 :foreground theme-bg :weight 'bold))
	    evil-emacs-state-tag    (propertize " E " 'face (list :background "orange" :foreground "black"))
	    evil-insert-state-tag   (propertize " I " 'face (list :background theme-color-2 :foreground theme-bg :weight 'bold))
	    evil-motion-state-tag   (propertize " M " 'face (list :background "blue" :foreground "white"))
	    evil-visual-state-tag   (propertize " V " 'face (list :background theme-color-2 :foreground "black"))
	    evil-operator-state-tag (propertize " O " 'face (list :background "purple"))))
    )
  ;set tags format after frame creation so ensure that the theme has been initialized
  (if (daemonp)
      (add-hook 'after-make-frame-functions (lambda (frame) (select-frame frame) (ftzm/format-evil-tags)))
    (ftzm/format-evil-tags))

  :config
  (setq evil-insert-state-message nil) ;; no echo area message on insert mode

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

  ; When I need training
  ; (define-key evil-normal-state-map (kbd "h") (lambda () (interactive) (message "no!")))
  ; (define-key evil-normal-state-map (kbd "j") (lambda () (interactive) (message "no!")))
  ; (define-key evil-normal-state-map (kbd "k") (lambda () (interactive) (message "no!")))
  ; (define-key evil-normal-state-map (kbd "l") (lambda () (interactive) (message "no!")))
  ; (define-key evil-normal-state-map (kbd "<up>") (lambda () (interactive) (message "no!")))
  ; (define-key evil-normal-state-map (kbd "<down>") (lambda () (interactive) (message "no!")))
  ; (define-key evil-normal-state-map (kbd "<left>") (lambda () (interactive) (message "no!")))
  ; (define-key evil-normal-state-map (kbd "<right>") (lambda () (interactive) (message "no!")))

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
  :diminish evil-collection-unimpaired-mode
  :config
  ;; add more as need by this pattern
  (evil-collection-init 'dired)
  (evil-collection-init 'vterm)
  (evil-collection-init 'emms)
  (evil-collection-init 'mu4e)
  (evil-collection-init 'magit)
  (evil-collection-init 'mpdel)
  ;(evil-collection-init 'org)
  (evil-collection-init 'xref)
  )

(use-package evil-org
  :straight t
  :after org
  :hook (org-mode . (lambda ()
		      (evil-org-mode)
		      ; This gets unbound in the terminal for some reason
		      (define-key evil-normal-state-map (kbd "TAB") 'org-cycle)
		      ))
  :config
  (require 'evil-org-agenda)
  ;(evil-org-agenda-set-keys)
  )

(use-package evil-surround
  :straight t
  :config
  (global-evil-surround-mode 1)
  )


(use-package evil-string-inflection
   :straight t)

;; (use-package evil-terminal-cursor-changer
;;   :straight t
;;   :hook (tty-setup . evil-terminal-cursor-changer-activate))

;; (use-package term-cursor
;;   :straight (term-cursor
;; 	     :type git
;; 	     :repo "denrat/term-cursor.el"
;; 	     :host github)
;;   :config
;;   (global-term-cursor-mode))

(provide 'init-evil)
