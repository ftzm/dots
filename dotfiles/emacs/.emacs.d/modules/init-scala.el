(use-package lsp-ui
  :straight t
  :config
  (setq lsp-ui-doc-enable nil)
  )

(use-package scala-mode
  :straight t
  :mode ".scala\\'"
  :interpreter
    ("scala" . scala-mode)
  :hook ((scala-mode . flycheck-mode)
	 (scala-mode . yas-minor-mode)
	 ; This is an ugly hack to stop an error about an invalid hanler in
	 ; file-name-handler alist that prevents visiting library source files
	 ; in scala.
	 (scala-mode . (lambda () (setq file-name-handler-alist nil)))
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
  :diminish lsp-lens-mode
  :diminish lsp-mode
  :config
  (setq lsp-prefer-flymake nil)
  (setq lsp-file-watch-threshold 100000)

  (defun db/lsp-treemacs-symbols-toggle ()
    "Toggle the lsp-treemacs-symbols buffer."
    (interactive)
    (if (get-buffer "*LSP Symbols List*") (kill-buffer "*LSP Symbols List*")
      (progn (lsp-treemacs-symbols)
             (other-window -1))))

  (defun ftzm/lsp-treemacs-errors-list-toggle ()
    "Toggle the lsp-treemacs-errors-list buffer"
    (interactive)
    (let ((error-window (get-buffer-window "*LSP Error List*")))
      (if error-window
	  (quit-window nil error-window)
	(lsp-treemacs-errors-list))))

  ;disable eldoc from showing type info automatically since it's slow
  (setq lsp-eldoc-enable-hover nil)

  (setq lsp-headerline-breadcrumb-enable nil)

  (defun lsp-describe-thing-at-point-minibuffer ()
  "Display the type signature and documentation of the thing at
point in the minibuffer."
  (interactive)
  (let ((contents (-some->> (lsp--text-document-position-params)
                    (lsp--make-request "textDocument/hover")
                    (lsp--send-request)
                    (lsp:hover-contents))))
    (if (and contents (not (equal contents "")))
        (message (string-trim-right (lsp--render-on-hover-content contents t)))
      (lsp--info "No content at point."))))

  )

(use-package lsp-metals
  :straight (lsp-metals
	     :type git
	     :repo "emacs-lsp/lsp-metals"
	     :host github)
  :config (setq lsp-metals-treeview-show-when-views-received nil))



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

(use-package lsp-ivy
  :straight t)

(defun scala-hydra-header ()
    (let ((workspace (car (lsp-workspaces))))
      (if workspace
	  (format "root:   %s\n server: %s" (lsp--workspace-root workspace) (lsp--workspace-print workspace))
	"No active workspace")))

(pretty-hydra-define scala-hydra
  (:color blue
   :quit-key "q"
   :title (scala-hydra-header))
  ("Commands" (( "f" lsp-format-buffer "format buffer")
	       ( "e" ftzm/lsp-treemacs-errors-list-toggle "toggle error list"))
   "LSP" (( "s" lsp "start lsp")
	  ( "i" lsp-metals-build-import "metals build+import")
	  ( "d" lsp-describe-thing-at-point-minibuffer "describe thing at point"))))

(evil-define-key 'normal scala-mode-map (kbd ",") 'scala-hydra/body)

(provide 'init-scala)
