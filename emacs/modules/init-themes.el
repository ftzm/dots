(use-package gruvbox-theme
  :straight t
  :config
  (load-theme 'gruvbox t)
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
  (with-eval-after-load 'powerline-themes
    (set-face-background 'powerline-inactive2 (face-attribute
					       'powerline-inactive1 :background))
    )
   )

(provide 'init-themes)
