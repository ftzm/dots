(use-package emms
  :config
  (require 'emms-setup)
  (emms-all)
  (emms-default-players)
   (require 'emms-info-libtag) ;;; load functions that will talk to emms-print-metadata which in turn talks to libtag and gets metadata
  (setq emms-info-functions '(emms-info-mp3info)) ;;; make sure libtag is the only thing delivering metadata
  (setq emms-info-asynchronously t)
  )

(provide 'init-music)
