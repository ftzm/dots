;;;;; Startup optimizations

;;;;;; Set garbage collection threshold

;; From https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/

(setq gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold (* 1024 1024 100))

;;;;;; Set file-name-handler-alist

;; Also from https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/

(setq file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;;;;;; Set deferred timer to reset them

(run-with-idle-timer
 5 nil
 (lambda ()
   (setq gc-cons-threshold gc-cons-threshold-original)
   (setq file-name-handler-alist file-name-handler-alist-original)
   (makunbound 'gc-cons-threshold-original)
   (makunbound 'file-name-handler-alist-original)
   (message "gc-cons-threshold and file-name-handler-alist restored")))


;; Disable some GUI distractions. We set these manually to avoid starting
;; the corresponding minor modes.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . nil) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)

(setq frame-inhibit-implied-resize t)

;; Set up fonts early.
(set-face-attribute 'default
                    nil
                    :height 120
                    :family "Iosevka Lig Medium")
(set-face-attribute 'variable-pitch
                    nil
                    :family "DejaVu Sans")
(setq-default line-spacing 0.1)

;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)
