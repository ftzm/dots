(use-package htmlize
  :straight t)

(setq org-edit-src-content-indentation 0)

(use-package typo
  :straight t)

(use-package visual-fill-column
  :disabled
  :straight t)

(use-package poly-org
  :disabled t
  :straight t
  )

(use-package project
  :config
  (project--read-project-list)
  (defun switch-persp-project ()
    (interactive "")
    (let ((projects project--list))
      (if projects (let* ((project
			   (completing-read "Project: " projects))
			  (persp-name
			   (file-name-nondirectory (substring project 0 -1))))
		     (persp-switch persp-name)
		     (project-switch-project project))
	(user-error "There are no known projects")))))

(provide 'literate-config)
