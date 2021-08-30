(setq package-enable-at-startup nil)

;;bootstrap straight
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 4))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; emacs 26 bug, remove with 27
(straight--package-built-in-p 'nadvice)
(puthash 'nadvice t straight--cached-built-in-packages)

;;set up use-package
(straight-use-package 'use-package)
;;used by use-package's :diminish
(straight-use-package 'diminish)
;;used by use-package's :bind variants
(straight-use-package 'bind-key)

(setq load-prefer-newer t)
(add-to-list 'load-path "/home/ftzm/.dots/emacs/src/emacs.d/")

;for profiling startup
(use-package esup
  :straight t
  :commands esup)

(provide 'init-straight)
