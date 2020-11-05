(use-package scala-mode
  :straight t
  :mode ".scala\\'"
  :interpreter
    ("scala" . scala-mode)
  :hook ((scala-mode . flycheck-mode)
	 ))

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :straight t
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false"))
)

(use-package lsp-mode
  :straight t
  :hook ((lsp-mode . lsp-lens-mode)(scala-mode . lsp))
  :config (setq lsp-prefer-flymake nil))

(use-package lsp-metals
  :straight (lsp-metals
	     :type git
	     :repo "emacs-lsp/lsp-metals"
	     :host github)
  :config (setq lsp-metals-treeview-show-when-views-received t))

(use-package lsp-ui
  :straight t)

;; Add company-lsp backend for metals
(use-package company-lsp
  :straight t)

;; Use the Debug Adapter Protocol for running tests and debugging
(use-package posframe
  ;; Posframe is a pop-up tool that must be manually installed for dap-mode
  :straight t)

(use-package dap-mode
  :straight t
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode)
  )


(provide 'init-scala)
