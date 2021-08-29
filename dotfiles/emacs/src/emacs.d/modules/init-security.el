(use-package epa-file
  :init
    (custom-set-variables '(epg-gpg-program))
    (setq epg-gpg-program   (executable-find "gpg2"))
    (epa-file-enable)
  )

(provide 'init-security)
