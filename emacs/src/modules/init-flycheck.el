(use-package flycheck
  :straight t
  :diminish "S"
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  (setq flycheck-display-errors-delay 0.1)

  ;; Don't show indicators in the fringe
  (setq flycheck-indication-mode nil)

  (setq flycheck-highlighting-mode 'lines) ;;highlight whole line

  (add-hook 'sh-mode-hook 'flycheck-mode)

  )

(provide 'init-flycheck)
