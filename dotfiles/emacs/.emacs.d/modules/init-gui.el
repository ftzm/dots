(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;
(setq inhibit-startup-screen t)
;
;;Set font
;; >>= :: <*> == /= >> {-#
(setq my-font (format "Iosevka Lig Medium-%s" (or (getenv "FONT_SIZE") "16")))
;;to set in existing window:
(set-frame-font my-font nil t)

;(custom-set-faces '(default ((t (:family "Iosevka Lig" :foundry "unknown" :slant normal :weight normal :height 200 :width normal :inverse-video nil :box nil :strike-through nil :overline nil :underline nil)))))
;(custom-set-faces '(default ((t (:family "JetBrains Mono" :foundry "JB  " :slant normal :weight normal :height 95 :width normal)))))
;
;; this only works on startup
;(add-to-list 'default-frame-alist '(font . (format "Iosevka Lig Medium-%s" (or (getenv "FONT_SIZE") "16"))))
;
;; (set-fontset-font t 'japanese-jisx0208
;;                   (font-spec :family "IPAGothic" :size 24))
;
(setq-default line-spacing 0.1)
;
;; no blink
(blink-cursor-mode 0)
;;; no blink in term
(setq visible-cursor nil)
;;; highlint current line
(global-hl-line-mode)
;
;;(setq-default display-line-numbers-current-absolute t
;;              display-line-numbers-width 4
;;              display-line-numbers-widen t)
;;(global-display-line-numbers-mode)
;
;

(setq initial-scratch-message "Welcome to Emacs.")

(setq-default cursor-in-non-selected-windows nil)

;; automatically balances windows when  splitting.
(setq window-combination-resize t)

; don't highlight bookmarks orange
(setq bookmark-fontify nil)

(provide 'init-gui)
