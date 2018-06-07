(use-package persp-mode
  :straight t
  :init
  ;; switch off animation, for restore
  (setq wg-morph-on nil)
  :config
  (setq persp-autokill-buffer-on-remove 'kill-weak)
  (add-hook 'after-init-hook #'(lambda () (persp-mode 1)))

  (with-eval-after-load "ivy"
   (add-hook 'ivy-ignore-buffers
             #'(lambda (b)
                 (when persp-mode
                   (let ((persp (get-current-persp)))
                     (if persp
                         (not (persp-contain-buffer-p b persp))
                       nil)))))

   (setq ivy-sort-functions-alist
         (append ivy-sort-functions-alist
                 '((persp-kill-buffer   . nil)
                   (persp-remove-buffer . nil)
                   (persp-add-buffer    . nil)
                   (persp-switch        . nil)
                   (persp-window-switch . nil)
	    (persp-frame-switch  . nil)))))

  )

(provide 'init-persp)
