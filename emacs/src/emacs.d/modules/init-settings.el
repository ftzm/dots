;------------------------------------------------------------------------------
; File handling
;------------------------------------------------------------------------------

(setq vc-follow-symlinks t)

(setq create-lockfiles nil)

(use-package undo-tree
  :straight t
  :init
  (global-undo-tree-mode)
  :diminish undo-tree-mode
  )

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;(savehist-mode 1)

;; keeps a list of recently visisted files
;;(use-package recentf
  ;;:init
  ;;(recentf-mode 1)
  ;;:config
  ;;(setq recentf-max-menu-items 25)
  ;;)


;------------------------------------------------------------------------------
; Assorted
;------------------------------------------------------------------------------

;; Camel Case recognition, works with evil mode movement
(use-package subword
  :diminish subword-mode
  :config
  (add-hook 'prog-mode-hook 'subword-mode)
  (defun ora-javascript-hook ()
    (setq-local avy-subword-extra-word-chars nil))
  (add-hook 'js-mode-hook 'ora-javascript-hook)
  )

(setq-default fill-column 79)
(diminish 'auto-fill-function)

;(global-eldoc-mode -1)

(provide 'init-settings)
