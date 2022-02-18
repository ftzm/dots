(use-package dired+
  :straight t
  :commands dired-omit-mode
  :hook (dired-mode . dired-omit-mode)
  :config
  (set-face-attribute 'diredp-omit-file-name nil
       		    :strike-through nil)
  (setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..*$")
  )


(setq dired-listing-switches "-aBhl  --group-directories-first")

(provide 'init-dired)
