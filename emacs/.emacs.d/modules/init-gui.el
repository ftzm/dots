(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;Set font
(add-to-list 'default-frame-alist '(font . "Fira Mono Medium-16" ))
(set-fontset-font t 'japanese-jisx0208
                  (font-spec :family "IPAGothic" :size 24))

;; no blink
(blink-cursor-mode 0)
;; no blink in term
(setq visible-cursor nil)
;; highlint current line
(global-hl-line-mode)

(setq inhibit-startup-screen t)
(setq initial-scratch-message "￼
This work is licensed to you under version 2 of the GNU General Public License. Alternatively, you may choose to receive this work under any other license that grants the right to use, copy, modify, and/or distribute the work, as long as that license imposes the restriction that derivative works have to grant the same rights and impose the same restriction. For example, you may choose to receive this work under the GNU Free Documentation License, the CreativeCommons ShareAlike License, the XEmacs manual license, or similar licenses.

Please note our Privacy Statement.")

(provide 'init-gui)
