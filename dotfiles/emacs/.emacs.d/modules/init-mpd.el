(use-package mpdel
  :straight t
  :commands (mpdel-mode mpd-hydra/body)
  :config
  ;;; Override Playlist appearance

  (defun mpdel-tablist--song-format ()
  "Return `tabulated-list-format' value for songs."
  (vector (list "Title" 50 t)
          (list "#" 3 nil)
          (list "Album" 40 t)
          (list "Artist" 0 t)))

  (navigel-method mpdel navigel-entity-to-columns ((song libmpdel-song))
    (vector
     (propertize (or (libmpdel-entity-name song) "") 'face 'mpdel-tablist-song-name-face)
     (propertize (or (libmpdel-song-track song) "") 'face 'mpdel-tablist-track-face)
     (propertize (or (libmpdel-album-name song) "") 'face 'mpdel-tablist-album-face)
     (propertize (or (libmpdel-artist-name song) "") 'face 'mpdel-tablist-artist-face)))

  ; (use-package ivy-mpdel
  ; :straight t)

  (defun ftzm-mpd-composer-search (name)
    (mpdel-core-search-by-filter (format "(composer == '%s')" name)))

  (defun ftzm-mpd-composer-findadd (name)
    (libmpdel-send-command (format "findadd \"(composer == '%s')\"" name)))

  ;; (defun ftzm-ivy-composer-search ()
  ;;   (interactive)
  ;;   (libmpdel-send-command
  ;;    "list composer"
  ;;    (lambda (data)
  ;;      (ivy-read "Composer: " (mapcar (lambda (x)
  ;; 				      (cdr x)) data)
  ;; 	       :action (lambda (x)
  ;; 			 (ftzm-mpd-composer-search x))
  ;; 	       :caller 'composer-names))))


  (defun ftzm-embark-composer-search ()
    (interactive)
    (libmpdel-send-command
     "list composer  \"(Genre == \\\"Classical\\\")\""
     (lambda (data)
       (with-local-quit ;; I can't remember why this was necessary, maybe it's not?
         (ftzm-mpd-composer-search
	  (consult--read (seq-filter (lambda (x) (> (length x) 0)) (mapcar (lambda (x) (cdr x)) data))
		         :prompt "Composer: "
		         :category 'composer
		         ))))))

  (defun ftzm-embark-artist-search ()
    (interactive)
    (libmpdel-send-command
     ;"list artist  \"(Genre == \\\"Classical\\\")\""
     "list artist"
     (lambda (data)
       (with-local-quit
         (mpdel-core-search-by-artist
	  (consult--read (seq-filter (lambda (x) (> (length x) 0)) (mapcar (lambda (x) (cdr x)) data))
		         :prompt "Artist: "
		         :category 'artist
		         ))))))

  (defun prepare-candidates (data)
    (seq-filter (lambda (x) (> (length x) 0)) (mapcar (lambda (x) (cdr x)) data)))

  (defun select-candidate-callback (candidates prompt category)
    ;; `with-local-quit` is necessary to avoid getting an error when quitting
    ;; the completion in a callback
    (consult--read (prepare-candidates candidates)
				    :prompt prompt
				    :category 'category))

  (defun list-non-classical-artists (fun)
    (with-local-quit
      (libmpdel-send-command "list artist \"(Genre != \\\"Classical\\\")\"" fun)))

  (defun list-artist-albums (artist fun)
    (with-local-quit
      (libmpdel-send-command
       (format  "list album \"(Artist == \\\"%s\\\")\"" artist)
       fun)))

  (require 'libmpdel)
  (defun ftzm-embark-artist-album-search ()
    (interactive)
    (list-non-classical-artists
     (lambda (artist-data)
       (let ((artist-name (select-candidate-callback artist-data "Artist: " 'artist)))
	 (list-artist-albums
	  artist-name
	  `(lambda (album-data)
	     (with-local-quit
	       (let* ((album-name (select-candidate-callback album-data "Album: " 'album))
		      (artist (libmpdel--artist-create :name ,artist-name))
		      (album (libmpdel--album-create :name album-name :artist artist))
		      )
		 (libmpdel-playlist-add album 'current-playlist)
				;	;(mpdel-core-search-by-album)
		 ))))))))

  (defun ftzm-mpd-format-time (data)
    (format "%s / %s" (libmpdel-time-to-string (cdr (assq 'elapsed data)))
	    (libmpdel-time-to-string (cdr (assq 'duration data)))))

  (defun ftzm-mpd-get-play-time ()
    (let* ((output "null")
	   (to-wait 100)
	   (was-set nil)
	   (watch-func '(lambda
			  (&rest
			   args)
			  (setq was-set t))))
      (add-variable-watcher 'output watch-func)
      (libmpdel-send-command "status" (lambda (data)
				        (setq output (ftzm-mpd-format-time data))))
      (while (and (not was-set)
		  (> to-wait 0))
        (setq to-wait (1- to-wait))
        (sit-for 0.001))
      (if (< to-wait 1)
	  (setq output "~"))
      (remove-variable-watcher 'output watch-func) output))

  ;(remove-variable-watcher 'output (lambda (&rest args) (setq was-set t)))
  ;(get-variable-watchers 'output)

  (defun ftzm-mpd-status ()
    (if libmpdel--current-song
        (let ((play-time (ftzm-mpd-get-play-time))
	      (playing (libmpdel-play-state))
	      (current-song (if libmpdel--current-song (libmpdel--song-name
						        libmpdel--current-song) ""))
	      (artist (if libmpdel--current-song  (libmpdel--artist-name
						   (libmpdel--album-artist
						    (libmpdel--song-album
						     libmpdel--current-song))) "")))
	  (format "[%s] %s \n %s\n %s" playing play-time current-song artist))
      "No song selected."))

  (defun ftzm-mpd-repeat ()
    (interactive)
    (if libmpdel--repeat
        (libmpdel-playback-unset-repeat)
      (libmpdel-playback-set-repeat)))

  (defun ftzm-mpd-single ()
    (interactive)
    (if (eq 'forever libmpdel--single)
        (libmpdel-playback-set-single-never)
      (libmpdel-playback-set-single-forever)))

  (defun ftzm-mpd-random ()
    (interactive)
    (if libmpdel--random
        (libmpdel-playback-unset-random)
      (libmpdel-playback-set-random)))

  (defun ftzm-mpd-clear-playlist ()
    (interactive)
    (libmpdel-current-playlist-replace '()))

  (pretty-hydra-define mpd-hydra
    (:color blue
     :quit-key "q"
     :title (ftzm-mpd-status))
    ("Playback" (( "p" libmpdel-playback-play-pause "play-pause")
	         ( "<" mpdel-song-normal-decrement "skip backward" :exit nil)
	         ( ">" mpdel-song-normal-increment "skip forward" :exit nil)
	         ( "{" libmpdel-playback-previous "previous" :exit nil)
	         ( "}" libmpdel-playback-next "next" :exit nil))
     "Selection" (( "s" mpdel-playlist-open "open current playlist")
		  ( "C" ftzm-mpd-clear-playlist "clear playlist")
		  ( "c" ftzm-embark-composer-search "search composers")
		  ;( "a" ivy-mpdel-artists "search artists")
		  )
     "Toggles" (( "r" ftzm-mpd-repeat "repeat" :toggle (symbol-value 'libmpdel--repeat))
	         ( "S" ftzm-mpd-single "single" :toggle (eq 'forever libmpdel--single))
	         ( "R" ftzm-mpd-random "random" :toggle (symbol-value 'libmpdel--random)))))


  (evil-collection-define-key 'normal 'mpdel-core-map
    "m"  'tablist-mark-forward)

  )
(provide 'init-mpd)
