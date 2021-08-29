(use-package magit
  :straight t
  :defer t
  :config
  ;; just quit using q, goes to commit message which is useful
  ;;(defun magit-blame-toggle()
    ;;"WORKAROUND https://github.com/magit/magit/issues/1987"
    ;;(interactive)
    ;;(let* ((active (--filter (and (boundp it) (symbol-value it)) minor-mode-list)))
      ;;(if (member 'magit-blame-mode active)
          ;;(magit-blame-quit)
        ;;(magit-blame))))

  (define-key transient-map        (kbd "<escape>") 'transient-quit-one)
  (define-key transient-edit-map   (kbd "<escape>") 'transient-quit-one)
  (define-key transient-sticky-map (kbd "<escape>") 'transient-quit-seq)

  )

(use-package autorevert
  :diminish auto-revert-mode
  )

(provide 'init-magit)
