;------------------------------------------------------------------------------
; File handling
;------------------------------------------------------------------------------

(use-package undo-tree
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

(provide 'init-settings)
