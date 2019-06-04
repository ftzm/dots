(use-package anaconda-mode
  :straight t
  :diminish (anaconda-mode . "A")
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'auto-fill-mode)
  (add-hook 'python-mode-hook (lambda ()
  			      (flycheck-mode 1)
  			      (setq flycheck-checker 'python-mypy
  				    flycheck-checker-error-threshold 900)))

  (defun pyvenv-and-fly (directory)
    "open interactive menu to choose the virtualenv (choose venv root), then
     restart flycheck."
    (interactive "DActivate venv: ")
    (pyvenv-activate directory)
    (flycheck-mode 0)
    (flycheck-mode 1)
    )

  (define-prefix-command 'python-mode-keys)
  (evil-define-key 'normal python-mode-map (kbd ",") 'python-mode-keys)
  (define-key python-mode-keys "v" 'pyvenv-and-fly)
  (define-key python-mode-keys "d" 'dumb-jump-go)

  (set-variable 'flycheck-python-mypy-args '("--ignore-missing-imports" "--check-untyped-defs" "--follow-imports=skip"))

  ;(require 'auto-virtualenv)
  ;(add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)
  ;
  ;(declare-function python-shell-calculate-exec-path "python")
  ;
  ;(defun flycheck-virtualenv-executable-find (executable)
  ;  "Find an EXECUTABLE in the current virtualenv if any."
  ;  (if (bound-and-true-p python-shell-virtualenv-root)
  ;      (let ((exec-path (python-shell-calculate-exec-path)))
  ;        (executable-find executable))
  ;    (executable-find executable)))
  ;
  ;(defun flycheck-virtualenv-setup ()
  ;  "Setup Flycheck for the current virtualenv."
  ;  (setq-local flycheck-executable-find #'flycheck-virtualenv-executable-find))
  ;
  ;(add-hook 'python-mode-hook #'flycheck-virtualenv-setup)

  )

(use-package pyvenv
  :straight t
  )

(use-package company-anaconda
  :straight t
  )

(use-package blacken
  :straight t
  )

(use-package python-pytest
  :straight t
  )

(provide 'init-python)
