(setq inhibit-startup-screen t)

; ;;Set font
; ;; >>= :: <*> == /= >> {-#
; (setq my-font (format "Iosevka Lig Medium-12" (or (getenv "FONT_SIZE") "16")))
; ;;to set in existing window:
; (set-frame-font my-font nil t)

;; no blink
(blink-cursor-mode 0)
;;; no blink in term
(setq visible-cursor nil)
;;; highlint current line
(global-hl-line-mode)

;;(setq-default display-line-numbers-current-absolute t
;;              display-line-numbers-width 4
;;              display-line-numbers-widen t)
;;(global-display-line-numbers-mode)

(set-face-attribute 'mode-line-active nil
		      :inherit 'mode-line)

(setq initial-scratch-message "Welcome to Emacs.")

(setq-default cursor-in-non-selected-windows nil)

;; automatically balances windows when  splitting.
(setq window-combination-resize t)

; don't highlight bookmarks orange
(setq bookmark-fontify nil)

;; The below adds prettier buffer dividers in the terminal
;; it seems to cause problems when compiling sometimes.
(defun my-change-window-divider ()
  (let ((display-table (or buffer-display-table standard-display-table)))
    (set-display-table-slot display-table 5 ?│)
    (set-window-display-table (selected-window) display-table)))


(add-hook 'window-configuration-change-hook 'my-change-window-divider)

(provide 'init-gui)
