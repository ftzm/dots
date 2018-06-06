(use-package magit
  :straight t
  :config
  (defun magit-blame-toggle()
    "WORKAROUND https://github.com/magit/magit/issues/1987"
    (interactive)
    (let* ((active (--filter (and (boundp it) (symbol-value it)) minor-mode-list)))
      (if (member 'magit-blame-mode active)
          (magit-blame-quit)
        (magit-blame nil buffer-file-name))))

  )

(use-package autorevert
  :diminish auto-revert-mode
  )

(provide 'init-magit)
