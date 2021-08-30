(use-package slime
  :straight t
  :mode ("\\.cl\\'" . slime-mode)
  :config
  (setq slime-contribs '(slime-fancy)))

(use-package paredit
  :straight t
  :diminish "pr"
  :hook ((emacs-lisp-mode
	  lisp-interaction-mode ; enable in the *scratch* buffer
	  list-mode
	  eval-expression-minibuffer-setup) . paredit-mode))

(provide 'init-lisp)
