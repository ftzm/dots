(use-package flycheck
  :straight t
  :hook ((json-mode) . flycheck-mode)
  :diminish "S"
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  (setq flycheck-display-errors-delay 0.1)

  ;; Don't show indicators in the fringe
  (setq flycheck-indication-mode nil)

  (setq flycheck-highlighting-mode 'columns) ;;highlight whole line

  (add-hook 'sh-mode-hook 'flycheck-mode)

  ;; This ensures that direnv loads before flycheck checks syntax, so that you
  ;; that checkers are found in the environment. Otherwise flycheck effectively
  ;; fails to start for the buffer
  (add-hook 'flycheck-before-syntax-check-hook 'direnv-update-environment)

  )

(provide 'init-flycheck)
