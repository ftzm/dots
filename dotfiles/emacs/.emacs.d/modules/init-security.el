(use-package epa-file
  :ensure nil ;; included with emacs
  :init
    ;(custom-set-variables '(epg-gpg-program))
    (setq epg-gpg-program   (executable-find "gpg2"))
    (epa-file-enable)
  )

(provide 'init-security)
