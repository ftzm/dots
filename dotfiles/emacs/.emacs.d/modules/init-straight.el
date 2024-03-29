;(setq package-enable-at-startup nil)

;; don't check for local modifications. Maybe check back that this isn't a bad
;; idea
;; https://www.reddit.com/r/emacs/comments/tb6vvt/decrease_emacs_launch_time_when_using_straightel/
(setq straight-check-for-modifications nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))



;;set up use-package
;(straight-use-package 'use-package)
;;used by use-package's :diminish
(straight-use-package 'diminish)
;;used by use-package's :bind variants
(straight-use-package 'bind-key)

(setq load-prefer-newer t)
(add-to-list 'load-path "/home/ftzm/.dots/emacs/src/emacs.d/")

; ;for profiling startup
; (use-package esup
;   :straight t
;   :commands esup)

(provide 'init-straight)
