(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;Set font
;to set in existing window:
(set-frame-font "Fira Mono Medium-16" nil t)

; this only works on startup
(add-to-list 'default-frame-alist '(font . "Fira Mono Medium-16" ))
(set-fontset-font t 'japanese-jisx0208
                  (font-spec :family "IPAGothic" :size 24))

;; no blink
(blink-cursor-mode 0)
;; no blink in term
(setq visible-cursor nil)
;; highlint current line
(global-hl-line-mode)

(setq-default ;display-line-numbers-current-absolute t
              display-line-numbers-width 4
              display-line-numbers-widen t)
;(global-display-line-numbers-mode)



(setq inhibit-startup-screen t)

(setq initial-scratch-message "Welcome to Emacs.")

(provide 'init-gui)
