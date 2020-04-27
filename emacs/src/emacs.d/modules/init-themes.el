(use-package gruvbox-theme
  :straight t
  :config
  (load-theme 'gruvbox t)

  (set-face-attribute 'mode-line nil
		      :background "#3c3836")
  (set-face-attribute 'mode-line-inactive nil
		      :background "#32302f"
		      :foreground "#928374")

  (with-eval-after-load 'ivy
    (set-face-attribute 'ivy-current-match nil
    :inherit nil
    :stipple nil
    :background nil
    :foreground "#8ec07c"
    :inverse-video nil
    :box nil
    :strike-through nil
    :overline nil
    :underline nil
    :slant 'normal
    :weight 'normal
    )
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
