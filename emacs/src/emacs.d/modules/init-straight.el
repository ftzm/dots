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

;;set up use-package
(straight-use-package 'use-package)
;;used by use-package's :diminish
(straight-use-package 'diminish)
;;used by use-package's :bind variants
(straight-use-package 'bind-key)

(provide 'init-straight)