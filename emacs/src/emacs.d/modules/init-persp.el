;; (use-package workgroups
;;   :straight t)

;; (use-package persp-mode
;;   :straight t
;;   :diminish
;;   :init
;;   ;; switch off animation, for restore
;;   (setq wg-morph-on nil)
;;   :config
;;   (setq persp-autokill-buffer-on-remove 'kill-weak)
;;   (add-hook 'after-init-hook #'(lambda () (persp-mode 1)))

;;   (setq persp-auto-resume-time 0) ; don't autoresume workspaces
;;   (setq persp-nil-name "*")

;;   (with-eval-after-load "ivy"
;;    (add-hook 'ivy-ignore-buffers
;;              #'(lambda (b)
;;                  (when persp-mode
;;                    (let ((persp (get-current-persp)))
;;                      (if persp
;;                          (not (persp-contain-buffer-p b persp))
;;                        nil)))))

;;    (setq ivy-sort-functions-alist
;;          (append ivy-sort-functions-alist
;;                  '((persp-kill-buffer   . nil)
;;                    (persp-remove-buffer . nil)
;;                    (persp-add-buffer    . nil)
;;                    (persp-switch        . nil)
;;                    (persp-window-switch . nil)
;; 	    (persp-frame-switch  . nil)))))

;;   (setq persp-use-workgroups t)
;;   )

(use-package perspective
  :straight t
  :config
  (persp-mode)

  ;; Override persp-mode-line to hide in global-mode-string
  (defun persp-mode-line () "")

  (setq fixed-persp-names '("mail"))

  (defun hydra-persp-names ()
    (let ((persps (sort (hash-table-values (perspectives-hash)) (lambda (a b)
								  (string< (persp-name a) (persp-name b)))))
          (current-name (persp-current-name))
          (parts '())
          (count 1))
      (dolist (persp persps (s-join "\n " (nreverse parts)))
	(let ((name (persp-name persp))
	      (bufcount (length (seq-filter (lambda (b) (and (buffer-live-p b) (not (minibufferp b)))) (persp-buffers persp)))))
	  (cond ((eq name current-name)
		 (push (propertize (format "%d %s (%d)" count name bufcount) 'face 'font-lock-warning-face) parts))
		(t
		 (push (format "%d %s (%d)" count name bufcount) parts))))
        (cl-incf count))))

  (defun persp-switch-label (n)
    (nth (- n 1) (persp-names)))

  (defun persp-switch-command (n)
    (persp-switch (nth (- n 1) (persp-names))))

  (pretty-hydra-define persp-hydra
    (:color blue :quit-key "q" :title "%s(hydra-persp-names)")
    ("Buffer"
     (( "k" (persp-remove-buffer (current-buffer)) "remove buffer")
      ( "a" (persp-add-buffer (current-buffer)) "remove buffer")
      ( "i" (persp-ibuffer (current-buffer)) "ibuffer")
      )
     "Persp"
     (("r" persp-rename "rename")
      ("C" (persp-kill (persp-current-name)) "kill"))
     "General"
     (("s" persp-switch "switch")
      ("p" persp-switch-last (format "prev [%s]"
				     (if (persp-last)
					 (persp-name (persp-last))
				       persp-initial-frame-name)))
      ("1" (persp-switch-command 1) nil)
      ("2" (persp-switch-command 2) nil)
      ("3" (persp-switch-command 3) nil)
      ("4" (persp-switch-command 4) nil)
      ("5" (persp-switch-command 5) nil)
      ("6" (persp-switch-command 6) nil)
      ("7" (persp-switch-command 7) nil)
      ("8" (persp-switch-command 8) nil)
      ("7" (persp-switch-command 7) nil)))))

  ;; Make the buffer menu pretty
  (ivy-set-display-transformer #'persp-ivy-switch-buffer #'+ivy-buffer-transformer)

(provide 'init-persp)
