(use-package gruvbox-theme
  :straight t
  :config
  (load-theme 'gruvbox t)

  (set-face-attribute 'mode-line nil
		      :background "#3c3836")
  (set-face-attribute 'mode-line-inactive nil
		      :background "#32302f"
		      :foreground "#928374")
  (set-face-foreground 'vertical-border "#3c3836")


    (custom-theme-set-faces
     'user
     ;'(variable-pitch ((t (:family "Source Serif Pro" :weight normal :height 1.1))))
     ;'(fixed-pitch ((t ( :family "Iosevka Lig" :slant normal :weight normal))))
     ;'(org-level-1 ((t ( ))))
     ;'(org-level-2 ((t ( :foreground nil :inherit org-level-2))))
     ;'(org-level-3 ((t ( :foreground nil :inherit org-level-3))))
     '(org-block-begin-line ((t ( :inherit font-lock-comment-face :background
				  nil :foreground "#504945"))))
     '(org-block-end-line ((t ( :inherit font-lock-comment-face :background nil
				:foreground "#504945"))))
     '(org-block ((t (:background nil))))
     )

  )

;(use-package sublime-themes
;  :straight t
;  :config
;  (load-theme 'brin t)
;  )

;(use-package solarized-theme
;  :straight t
;  :config
;  ;(load-theme 'solarized-light t)
;  )

;(use-package darktooth-theme
;  :straight t
;  :config
;  ;(load-theme 'darktooth t)
;  ;(darktooth-modeline)
;  )
;
;(use-package doom-themes
;  :straight t
;  :config
;  ;(load-theme 'doom-spacegrey t)
;  ;(load-theme 'darktooth t)
;  ;(darktooth-modeline)
;  )

(provide 'init-themes)
