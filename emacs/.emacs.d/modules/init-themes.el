(use-package gruvbox-theme
  :straight t
  :config
  (load-theme 'gruvbox t)
 ;'(ivy-current-match ((t (:foreground "#8ec07c" :underline nil :weight normal))))
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
  ;:height 98
  ;:width 'normal
  ;:foundry "unknown"
  ;:family "DejaVu Sans"
  )
   )

(provide 'init-themes)
