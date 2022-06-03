(use-package dired+
  :straight t
  :commands dired-omit-mode
  :hook (dired-mode . dired-omit-mode)
  :config
  (set-face-attribute 'diredp-omit-file-name nil
       		    :strike-through nil)
  (setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..*$")
  )

(use-package dired-launch
  :straight t
  :hook (dired-mode . dired-launch-mode)
  :config
  (define-key dired-launch-mode-map (kbd "C-c C-c") 'dired-launch-command))


(setq dired-listing-switches "-aBhl  --group-directories-first")

(provide 'init-dired)
