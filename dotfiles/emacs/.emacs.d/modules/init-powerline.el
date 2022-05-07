;(use-package doom-modeline
;  :straight t
;  :init (doom-modeline-mode 1)
;  )

(use-package powerline
  :straight t
  :config

  (setq flycheck-mode-line-prefix "")

  (defun ftzm/flycheck-mode-line-status-text (&optional status)
    "Get a text describing STATUS for use in the mode
line. STATUS defaults to `flycheck-last-status-change' if omitted
or nil."
  (let ((text (pcase (or status flycheck-last-status-change)
                (`not-checked "")
                (`no-checker "-")
                (`running "*")
                (`errored "!")
                (`finished
                 (let-alist (flycheck-count-errors flycheck-current-errors)
                   (if (or .error .warning .info)
                       (format "%s-%s-%s" (or .error 0) (or .warning 0) (or .info 0))
                     "")))
                (`interrupted ".")
                (`suspicious "?"))))
    (concat " " flycheck-mode-line-prefix text)))

  (defun powerline-ftzm-theme ()
  "Setup the default mode-line."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face0 (if active 'powerline-inactive0 'powerline-inactive0))
                          (face1 (if active 'powerline-inactive1 'powerline-inactive1))
                          (face2 (if active 'powerline-inactive1 'powerline-inactive1))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list
				(if active
				    (powerline-raw evil-mode-line-tag)
				  (powerline-raw (propertize evil-mode-line-tag
							     'face (list
								    :background
								    (face-attribute
								     'highlight
								     :background)
								    :foreground
								    (face-attribute 'default :background)
								    :weight 'bold))))
				(powerline-raw "%*" face0 'l)
                                     ;(powerline-buffer-id `(mode-line-buffer-id ,face0) 'l)
				     (powerline-raw (buffer-name) face0 'l)
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       (powerline-raw which-func-format face0 'l))
                                     (powerline-raw " " face0)
                                     ;(funcall separator-left face0 face1)
                                     (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
                                       (powerline-raw erc-modified-channels-object face1 'l))
                                     (powerline-major-mode face1 'l)
                                     (powerline-process face1)
                                     (powerline-minor-modes face1 'l)
                                     (powerline-narrow face1 'l)
                                     (powerline-raw " " face1)
                                     ;(funcall separator-left face1 face2)
				     (when (and (boundp 'flycheck-mode) flycheck-mode)
				       (powerline-raw
					(ftzm/flycheck-mode-line-status-text) face2))))
                          (rhs (list (powerline-raw global-mode-string face2 'r)
                                     ;(funcall separator-right face2 face1)
                                     ;(unless window-system
                                     ;  (powerline-raw (char-to-string #xe0a1) face1 'l))
                                     ;(funcall separator-right face1 face0)
                                     (powerline-raw "%l" face0 'l)
                                     (powerline-raw ":" face0 'l)
                                     (powerline-raw "%2c" face0 'r)
                                     (powerline-raw (if (display-graphic-p) "  " " ") face0 'r)
                                     )))
                     (concat (powerline-render lhs)
                             (powerline-fill face2 (powerline-width rhs))
                             (powerline-render rhs)))))))

  (defun powerline-ftzm-theme-bu ()
  "Setup the default mode-line."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face0 (if active 'powerline-active0 'powerline-inactive0))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (powerline-raw "%*" face0 'l)
                                     ;(powerline-buffer-id `(mode-line-buffer-id ,face0) 'l)
				     (powerline-raw (buffer-name) face0 'l)
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       (powerline-raw which-func-format face0 'l))
                                     (powerline-raw " " face0)
                                     (funcall separator-left face0 face1)
                                     (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
                                       (powerline-raw erc-modified-channels-object face1 'l))
                                     (powerline-major-mode face1 'l)
                                     (powerline-process face1)
                                     (powerline-minor-modes face1 'l)
                                     (powerline-narrow face1 'l)
                                     (powerline-raw " " face1)
                                     (funcall separator-left face1 face2)
				     (when (and (boundp 'flycheck-mode) flycheck-mode)
				       (powerline-raw
					(ftzm/flycheck-mode-line-status-text) face2))))
                          (rhs (list ;(powerline-raw global-mode-string face2 'r)
                                     (funcall separator-right face2 face1)
                                     ;(unless window-system
                                     ;  (powerline-raw (char-to-string #xe0a1) face1 'l))
                                     (powerline-raw "%l" face1 'l)
                                     (powerline-raw ":" face1 'l)
                                     (powerline-raw "%c" face1 'r)
                                     (funcall separator-right face1 face0)
                                     (powerline-raw " " face0)
                                     (powerline-raw "%p" face0 'r)
                                     (powerline-fill face0 0)
                                     )))
                     (concat (powerline-render lhs)
                             (powerline-fill face2 (powerline-width rhs))
                             (powerline-render rhs)))))))

  (setq powerline-default-separator nil)

  (powerline-ftzm-theme)
  ;(powerline-default-theme)
  )

;(use-package spaceline
;  :straight t
;  )
;
;(use-package spaceline-config
;  :config
;  (if (display-graphic-p)
;      (progn
;  	(setq powerline-default-separator nil)
;  	)
;    (progn
;      ;(setq powerline-default-separator nil)
;      (setq powerline-default-separator nil)
;      )
;    )
;
;  (setq spaceline-minor-modes-separator "•")
;
;  (spaceline-emacs-theme)
;
;  (spaceline-toggle-buffer-size-off)
;  (spaceline-toggle-hud-off)
;  (spaceline-toggle-buffer-position-off)
;  (spaceline-toggle-buffer-encoding-abbrev-off)
;  (spaceline-toggle-version-control-off)
;  (spaceline-toggle-evil-state-off)
;  (spaceline-toggle-workspace-number-off)
;  (spaceline-toggle-projectile-root-on)
;  (spaceline-toggle-persp-name-off)
;
;  (set-face-attribute 'spaceline-flycheck-info nil :foreground 'unspecified :inherit 'font-lock-variable-name-face)
;  (set-face-attribute 'spaceline-flycheck-warning nil :foreground 'unspecified :inherit 'font-lock-function-name-face)
;  (set-face-attribute 'spaceline-flycheck-error nil :foreground 'unspecified :inherit 'font-lock-keyword-face)
;
;  ;; for gruvbox
;  (set-face-attribute 'powerline-active1 nil :foreground 'unspecified :inherit 'mode-line)
;  (set-face-attribute 'powerline-inactive2 nil :background 'unspecified :inherit 'powerline-inactive1)
;
;  ;; Redefine version control segment because it was too verbose
;  ;;(spaceline-define-segment version-control
;  ;;"Version control information."
;  ;;(when vc-mode
;    ;;(powerline-raw
;     ;;(s-trim (concat (replace-regexp-in-string "Git.?" "" vc-mode)
;                     ;;(when (buffer-file-name)
;                       ;;(pcase (vc-state (buffer-file-name))
;                         ;;(`up-to-date "")
;                         ;;(`edited "+")
;                         ;;(`added " Add")
;                         ;;(`unregistered " ??")
;                         ;;(`removed " Del")
;                         ;;(`needs-merge " Con")
;                         ;;(`needs-update " Upd")
;                         ;;(`ignored " Ign")
;			 ;;(_ " Unk"))))))))
;
;  )

;(use-package    feebleline
;  :straight       t
;  :config       (setq feebleline-msg-functions
;                      '((feebleline-line-number         :post "" :fmt "%5s")
;                        (feebleline-column-number       :pre ":" :fmt "%-2s")
;                        (feebleline-file-directory      :face feebleline-dir-face :post "")
;                        (feebleline-file-or-buffer-name :face font-lock-keyword-face :post "")
;                        (feebleline-file-modified-star  :face font-lock-warning-face :post "")
;                        (feebleline-git-branch          :face feebleline-git-face :pre " : ")
;                        ((lambda () "test")          :face feebleline-git-face :pre " : ")
;                        (ftzm/flycheck-mode-line-status-text     :face feebleline-git-face :pre " : ")
;                        (feebleline-project-name        :align right)))
;                (feebleline-mode 0))

(provide 'init-powerline)
;;; init-spaceline.el ends here
