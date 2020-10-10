(use-package emms
  :config
  (require 'emms-setup)
  (emms-all)
  (emms-default-players)
   (require 'emms-info-libtag) ;;; load functions that will talk to emms-print-metadata which in turn talks to libtag and gets metadata
  (setq emms-info-functions '(emms-info-libtag)) ;;; make sure libtag is the only thing delivering metadata
  (setq emms-info-asynchronously t)

  (defun minute-string (total-seconds)
    (let ((minutes (/ total-seconds 60))
	  (seconds (% total-seconds 60)))
      (format "%d:%02d" minutes seconds)
      ))

  (defun emms-show-status ()
    (if (emms-playlist-current-selected-track)
	(let* ((total-playing-time (emms-track-get
				    (emms-playlist-current-selected-track)
				    'info-playing-time))
	       (artist (emms-track-get
			(emms-playlist-current-selected-track)
			'info-artist))
	       (title (emms-track-get
		       (emms-playlist-current-selected-track)
		       'info-title))
	       (play-status (cond ( (and emms-player-paused-p
					 emms-player-playing-p) "Paused")
				  ( emms-player-playing-p       "Playing")
				  ( t                           "Stopped")))
	       (elapsed/total (/ (* 100 emms-playing-time) total-playing-time))
	       (elapsed-minutes (minute-string emms-playing-time))
	       (total-minutes (minute-string total-playing-time))
	       (bar (format "[%s] %s - %s \n %s / %s" play-status artist title  elapsed-minutes total-minutes ))) ;; double ampersand so we can embed it in another format string
	  bar)
      "No playlist"))

  (defun chunyang-emms-indicate-seek (_sec)
    (emms-show-progress)
      (sit-for 2))

  (defun raise-or-make (name creator)
    (let ((exists (member name (persp-names))))
      (persp-switch name)
      (when (not exists)
	(funcall creator))))

  (defun setup-music-workspace ()
    (interactive)
    (dired "/home/matt/music")
    (split-window-right)
    (other-window 1)
    (switch-to-buffer (emms-playlist-new))
    (emms-playlist-set-playlist-buffer)
    )

  (defun raise-music ()
    (interactive)
    (raise-or-make "music" 'setup-music-workspace))

  (defun music ()
    ()
    )

  (pretty-hydra-define music-hydra
    (:color blue :quit-key "q" :title "%s(emms-show-status)")
    ("Commands"
     (( "p" emms-pause "play/pause")
      ( "m" raise-music "music workspace"))))
  )

(provide 'init-music)
