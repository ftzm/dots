(use-package projectile
  :straight t
  :commands projectile-switch-project
  :diminish projectile-mode
  :after perspective
  :init
  (projectile-mode)
  :commands switch-persp-project
  :config
  ;(setq projectile-completion-system 'ivy) ;;requires ivy
  (setq projectile-completion-system 'default)

  ;; this improves speed by not being bogged down by zsh stuff
  (setq shell-file-name "/bin/sh")
  (setq projectile-enable-caching t)

  (setq projectile-globally-ignored-file-suffixes '("~" "#"))
  (setq projectile-use-git-grep t)

  (defun switch-persp-project ()
    (interactive "")
    (let ((projects (projectile-relevant-known-projects)))
      (if projects (let* ((project (completing-read "Project: " projects))
			  (persp-name (file-name-nondirectory (substring project 0 -1))))
		     (persp-switch persp-name)
		     (projectile-switch-project-by-name project nil))
	(user-error
	 "There are no known projects"))))

  )

;(use-package counsel-projectile
;  :straight t
;  :after (projectile, counsel)
;  )

(defun switch-persp-project ()
  (interactive "")
  (let ((projects project--list))
    (if projects (let* ((project (completing-read "Project: " projects))
		  (persp-name (file-name-nondirectory (substring project 0 -1))))
	     (persp-switch persp-name)
	     (project-switch-project project))
(user-error
 "There are no known projects"))))

(provide 'init-projectile)
