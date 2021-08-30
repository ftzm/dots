(use-package elfeed
  :straight t
  :bind (("C-x w" . elfeed-show))
  )

(use-package elfeed-org
  :straight t
  :after (elfeed)
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/org/elfeed.org")))
