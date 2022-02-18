;; just to make visual line mode available and diminished
(use-package simple
  :diminish visual-line-mode
  )

(use-package olivetti
  :straight t
  :diminish "O"
  :commands olivetti-mode
  :config
  (setq-default olivetti-body-width 80)
  (add-hook 'text-mode-hook #'olivetti-mode)


  ;; make olivetti not fuck up restoring window configurations with
  ;; persp mode
  ;; (with-eval-after-load "olivetti"
  ;;  (with-eval-after-load "persp-mode"
  ;;    (defvar persp-olivetti-buffers-backup nil)
  ;;    (add-hook 'persp-before-deactivate-functions
  ;;              #'(lambda (fow)
  ;;                  (dolist (b (mapcar #'window-buffer
  ;;                                     (window-list (selected-frame)
  ;;                                                  'no-minibuf)))
  ;;                    (with-current-buffer b
  ;;                      (when (eq 'olivetti-split-window-sensibly
  ;;                                split-window-preferred-function)
  ;;                        (push b persp-olivetti-buffers-backup)
  ;;                        (remove-hook 'window-configuration-change-hook
  ;;                                     #'olivetti-set-environment t)
  ;;                        (setq-local split-window-preferred-function nil)
  ;;                        (olivetti-reset-all-windows))))))
  ;;    (add-hook 'persp-activated-functions
  ;;              #'(lambda (fow)
  ;;                  (dolist (b persp-olivetti-buffers-backup)
  ;;                    (with-current-buffer b
  ;;                      (setq-local split-window-preferred-function
  ;;                                  'olivetti-split-window-sensibly)
  ;;                      (add-hook 'window-configuration-change-hook
  ;;                                #'olivetti-set-environment nil t)))
  ;;                  (setq persp-olivetti-buffers-backup nil))))
  ;; )
  )

(provide 'init-prose)
