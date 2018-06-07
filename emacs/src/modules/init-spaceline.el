(use-package spaceline
  :straight t
  )

(use-package spaceline-config
  :after (spaceline)
  :config
  (if (display-graphic-p)
      (progn
  	(setq powerline-default-separator 'slant)
  	)
    (progn
      ;(setq powerline-default-separator nil)
      (setq powerline-default-separator 'slant)
      )
    )

  (spaceline-spacemacs-theme)

  (spaceline-toggle-buffer-size-off)
  (spaceline-toggle-hud-off)
  (spaceline-toggle-buffer-position-off)
  (spaceline-toggle-buffer-encoding-abbrev-off)
  (spaceline-toggle-version-control-off)
  (spaceline-toggle-evil-state-off)
  (spaceline-toggle-workspace-number-on)
  (spaceline-toggle-projectile-root-on)

  (set-face-attribute 'spaceline-flycheck-info nil :foreground 'unspecified :inherit 'font-lock-variable-name-face)
  (set-face-attribute 'spaceline-flycheck-warning nil :foreground 'unspecified :inherit 'font-lock-function-name-face)
  (set-face-attribute 'spaceline-flycheck-error nil :foreground 'unspecified :inherit 'font-lock-keyword-face)

  ;; for gruvbox
  (set-face-attribute 'powerline-inactive2 nil :background 'unspecified :inherit 'powerline-inactive1)

  ;; Redefine version control segment because it was too verbose
  ;;(spaceline-define-segment version-control
  ;;"Version control information."
  ;;(when vc-mode
    ;;(powerline-raw
     ;;(s-trim (concat (replace-regexp-in-string "Git.?" "" vc-mode)
                     ;;(when (buffer-file-name)
                       ;;(pcase (vc-state (buffer-file-name))
                         ;;(`up-to-date "")
                         ;;(`edited "+")
                         ;;(`added " Add")
                         ;;(`unregistered " ??")
                         ;;(`removed " Del")
                         ;;(`needs-merge " Con")
                         ;;(`needs-update " Upd")
                         ;;(`ignored " Ign")
			 ;;(_ " Unk"))))))))

  (spaceline-compile)

  )

(provide 'init-spaceline)
