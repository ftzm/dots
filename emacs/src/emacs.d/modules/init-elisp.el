(use-package elisp-mode
  :hook ((emacs-lisp-mode . (lambda () (setq mode-name "elisp")))
	 (emacs-lisp-mode . auto-fill-mode)
	 (emacs-lisp-mode . eldoc-mode))
  :diminish "elisp")

(use-package eldoc
  :diminish "dc")

(use-package elisp-format
  :straight t)

(provide 'init-elisp)
