(use-package magit
  :straight t
  :config
  ;; just quit using q, goes to commit message which is useful
  ;;(defun magit-blame-toggle()
    ;;"WORKAROUND https://github.com/magit/magit/issues/1987"
    ;;(interactive)
    ;;(let* ((active (--filter (and (boundp it) (symbol-value it)) minor-mode-list)))
      ;;(if (member 'magit-blame-mode active)
          ;;(magit-blame-quit)
        ;;(magit-blame))))

  )

(use-package autorevert
  :diminish auto-revert-mode
  )

(use-package evil-magit
  :straight t
  )

(provide 'init-magit)
