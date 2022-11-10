;------------------------------------------------------------------------------
; File handling
;------------------------------------------------------------------------------

(setq vc-follow-symlinks t)

(setq create-lockfiles nil)

;; Emacs 28: Hide commands in M-x which do not work in the current mode.
;; Vertico commands are hidden in normal buffers.
(setq read-extended-command-predicate
      #'command-completion-default-include-p)

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(savehist-mode 1)

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

(global-eldoc-mode -1)

(provide 'init-settings)
