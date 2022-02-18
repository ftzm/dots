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
