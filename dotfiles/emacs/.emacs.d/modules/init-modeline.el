    ;(setq mode-line-format
          ;(list
	   ;" "
           ;;; value of current buffer name
           ;"%b"
	   ;" "
           ;;; value of current line number
           ;"%l:%c "
           ;;; value of `mode-name'
           ;"%m: "
           ;"-- user: "
           ;;; value of user
           ;(getenv "USER")))

(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length containing LEFT, and RIGHT aligned respectively."
  (let* ((available-width (- (window-total-width) (+ (length (format-mode-line left)) (length (format-mode-line right))))))
    (append left (list (format (format "%%%ds" available-width) "")) right)
    )
  )


(setq quiet-evil (propertize evil-mode-line-tag 'face (list :background theme-highlight :foreground theme-bg :weight 'bold)))
(put 'quiet-evil 'risky-local-variable t)

(setq ml-position '(:eval (list (propertize "%4l:" 'face (list :background  "#504945"))
				(propertize (format "%-3d" (current-column))
				'face (list :background "#504945")))))
(put 'ml-position 'risky-local-variable t)

(setq active-modeline (simple-mode-line-render
        ;; left
        (quote ("" evil-mode-line-tag " " mode-line-buffer-identification " %*"));
		;evil-mode-line-tag evil-state (s-trim-right (evil-state-property evil-state :tag t))))
        ;; right
        (quote (" " mode-line-frame-identification
		(seq-filter (lambda (x) (symbol-value (car x))) minor-mode-alist)
		" · "
		mode-name
		" · "
		"%3l:"
		(3 "%c")
		))
        ))
(setq inactive-modeline (simple-mode-line-render
        ;; left
        (quote ("" quiet-evil  " " mode-line-buffer-identification " %*"));
		;evil-mode-line-tag evil-state (s-trim-right (evil-state-property evil-state :tag t))))
        ;; right
        (quote (" " mode-line-frame-identification
		(seq-filter (lambda (x) (symbol-value (car x))) minor-mode-alist)
		" · "
		mode-name
		" %3l:"
		(3 "%c")))
        ))

;(setq-default
(setq-default mode-line-format
  '(:eval
      (if (eq ml-selected-window (selected-window))
	  active-modeline
	inactive-modeline)))

(provide 'init-modeline)
