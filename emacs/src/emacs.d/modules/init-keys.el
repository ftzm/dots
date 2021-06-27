(global-set-key (kbd "<escape>")      'keyboard-escape-quit)

 (defhydra hydra-rotate-menu (:color pink
                              :hint nil)
   "
 ^Layouts^                ^Maniplation^
 ^^^^^^^^-----------------------------------
 _h_: even horizontal     _l_: unmark
 _v_: even vertical       _r_: unmark up
 _H_: main horizontal
 _V_: main vertical
 _t_: tiled

 "
 ("h" rotate:even-horizontal)
 ("v" rotate:even-vertical)
 ("H" rotate:main-horizontal)
 ("V" rotate:main-vertical)
 ("t" rotate:tiled)
 ("l" rotate-layout)
 ("r" rotate-window)
 ("<escape>" nil "cancel")
)

;; ;; Go to last (i.e. most recent) perspective
;; (setq ftzm/last-persp persp-nil-name)

;; (add-hook 'persp-before-switch-functions
;; 	  #'(lambda (next-pn &rest rest)
;; 	      (setq ftzm/last-persp (safe-persp-name (get-current-persp)))))

;; (defun ftzm/switch-last-persp ()
;;   (interactive)
;;   (persp-switch ftzm/last-persp))

;; (defun hydra-persp-names ()
;;   (let ((names (persp-names-current-frame-fast-ordered))
;;         (current-name (safe-persp-name (get-current-persp)))
;;         (parts '())
;;         (count 1))
;;     (dolist (name names (s-join "  " (nreverse parts)))
;;       (cond ((eq name current-name)
;;              (push (propertize (format "%d:%s" count name) 'face 'font-lock-warning-face) parts))
;;             (t
;;              (push (format "%d:%s" count name) parts)))
;;       (cl-incf count)))))

;; (use-package pretty-hydra
;;   :straight t)

;; (defun persp-switch-label (n)
;;   (nth (- n 1) (persp-names-current-frame-fast-ordered)))

;; (defun persp-switch-command (n)
;;   (persp-switch (nth (- n 1) (persp-names-current-frame-fast-ordered))))


;; (pretty-hydra-define persp-hydra
;;   (:color blue :quit-key "q" :title "%s(hydra-persp-names)")
;;   ("Buffer"
;;    (( "k" (persp-remove-buffer (current-buffer)) "remove buffer")
;;     ( "a" (persp-add-buffer (current-buffer)) "remove buffer"))
;;    "Persp"
;;    (("r" persp-rename "rename")
;;     ("C" (persp-kill (safe-persp-name (get-current-persp))) "kill"))
;;    "General"
;;    (("s" persp-frame-switch "switch")
;;     ("p" ftzm/switch-last-persp (format "prev [%s]" ftzm/last-persp))
;;     ("1" (persp-switch-command 1) nil)
;;     ("2" (persp-switch-command 2) nil)
;;     ("3" (persp-switch-command 3) nil)
;;     ("4" (persp-switch-command 4) nil)
;;     ("5" (persp-switch-command 5) nil)
;;     ("6" (persp-switch-command 6) nil)
;;     ("7" (persp-switch-command 7) nil)
;;     ("8" (persp-switch-command 8) nil)
;;     ("7" (persp-switch-command 7) nil)))

(use-package general
  :straight t
  :init
  ;; to override evil-collection
  (setq general-override-states '(insert
                                  emacs
                                  hybrid
                                  normal
                                  visual
                                  motion
                                  operator
                                  replace))
  :config
  (general-define-key
   :states '(normal visual motion)
   :keymaps '(override
              dired-mode-map
    	      magit-mode-map
	      magit-blame-mode-map
    	      evil-normal-state-map)
   "SPC"
   'leader-menu)

  (general-evil-setup)

  (general-nmap "c" ;; this must be defined after evil to bind c
              (general-key-dispatch 'evil-change
                "c" (general-simulate-key ('evil-change "e"))
                ))
  (general-vmap "c" 'evil-change)

  (define-prefix-command 'leader-menu)
  (define-key leader-menu (kbd "SPC") 'execute-extended-command)
  (define-key leader-menu (kbd "'") 'switch-to-previous-buffer)
  (define-key leader-menu (kbd ",") 'ace-window)
  (define-prefix-command 'apps-keys)
  (define-prefix-command 'toggle-keys)
  (define-key toggle-keys "b" 'default-text-scale-increase)
  (define-key toggle-keys "s" 'default-text-scale-decrease)
  (define-key leader-menu "a" 'apps-keys)
  (define-key leader-menu "t" 'toggle-keys)

  (define-prefix-command 'window-keys)
  (define-key leader-menu "w" 'window-keys)
  (general-define-key :keymaps 'window-keys
                      "v" 'split-window-right
                      "s" 'split-window-below
                      "h" 'evil-window-left
                      "j" 'evil-window-down
                      "k" 'evil-window-up
                      "l" 'evil-window-right
                      "H" 'evil-window-move-far-left
                      "J" 'evil-window-move-very-bottom
                      "K" 'evil-window-move-very-top
                      "L" 'evil-window-move-far-right
                      "r" 'hydra-rotate-menu/body
                      "R" 'evil-window-rotate-upwards
                      "d" 'delete-window
                      "w" 'evil-window-next)

  (define-prefix-command 'buffer-keys)
  (define-key leader-menu "b" 'buffer-keys)
  (general-define-key :keymaps 'buffer-keys
                      "d" 'kill-this-buffer
                      "e" 'eval-buffer
                      "k" 'evil-prev-buffer
                      "j" 'evil-next-buffer
                      "b" 'switch-to-buffer
                      "s" 'save-buffer
                      "f" 'find-file
                      "w" 'write-file
                      "r" 'consult-recent-file
                      "u" 'sudo-find-file
                      "U" 'sudo-this-file)

  (define-key toggle-keys "l" 'global-display-line-numbers-mode)

  (define-key apps-keys "d" 'dired)

  (eval-after-load 'dired
    '(progn
       (evil-define-key 'normal dired-mode-map
         "G" 'evil-goto-line
         "gg" 'evil-goto-first-line
         "K" 'dired-kill-subdir
         "S" 'dired-sort-toggle-or-edit
         "s" 'avy-goto-word-or-subword-1
         ")" 'dired-next-subdir
         "(" 'dired-prev-subdir)))

  (define-key leader-menu "s" 'persp-hydra/body)
  (define-key leader-menu "m" 'mpd-hydra/body)

  ;(define-key evil-normal-state-map "s" 'avy-goto-char-timer)
  ;(define-key evil-normal-state-map "s" 'avy-goto-char-flex)
  (define-key evil-normal-state-map "s" 'avy-goto-word-or-subword-1)

  (define-prefix-command 'ivy-keys)
  (define-key leader-menu "i" 'ivy-keys)
  (define-key ivy-keys "i" 'consult-imenu)
  (define-key ivy-keys "g" 'consult-line)

  (define-prefix-command 'magit-keys)
  (define-key leader-menu "g" 'magit-keys)
  (define-key magit-keys "s" 'magit-status)
  (define-key magit-keys "b" 'magit-blame)
  (define-key magit-keys "B" 'magit-blame-quit)

  (define-key toggle-keys "g" 'global-git-gutter-mode)

  (define-prefix-command 'projectile-keys)
  (define-key leader-menu "p" 'projectile-keys)
  (define-key projectile-keys "f" 'projectile-find-file)
  (define-key projectile-keys "b" 'projectile-switch-to-buffer)
  (define-key projectile-keys "p" 'switch-persp-project)
  (define-key projectile-keys "P" 'projectile-switch-project)
  (define-key projectile-keys "a" 'ftzm/consult-ripgrep)

  (define-prefix-command 'org-mode-keys)
  (evil-define-key 'normal org-mode-map (kbd ",") 'org-mode-keys)
  (define-key org-mode-keys "s" 'org-schedule)
  (define-key org-mode-keys "r" 'org-refile)
  (define-key org-mode-keys "t" 'org-set-tags-command)

  ;; Map for my custom ',' prefix.
  (define-prefix-command 'agenda-mode-map-keys)
  (define-key agenda-mode-map-keys "w" 'org-agenda-week-view)
  (define-key agenda-mode-map-keys "D" 'org-agenda-day-view)
  (define-key agenda-mode-map-keys "d" 'org-agenda-deadline)
  (define-key agenda-mode-map-keys "r" 'org-agenda-refile)
  (define-key agenda-mode-map-keys "s" 'org-agenda-schedule)
  (define-key agenda-mode-map-keys "p" 'org-agenda-priority)
  (define-key agenda-mode-map-keys "f" 'org-agenda-filter-by-tag)
  (define-key agenda-mode-map-keys "cs" 'agenda-remove-schedule)

  (evil-define-key 'normal org-agenda-mode-map
    (kbd "<DEL>") 'org-agenda-show-scroll-down
    (kbd "<RET>") 'org-agenda-switch-to
    (kbd "\t") 'org-agenda-goto
    "\C-n" 'org-agenda-next-line
    "\C-p" 'org-agenda-previous-line
    "\C-r" 'org-agenda-redo
    "a" 'org-agenda-archive-default-with-confirmation
  				;b
    "c" 'org-agenda-goto-calendar
    "d" 'org-agenda-day-view
    "e" 'org-agenda-set-effort
    "f"  'org-agenda-later
    "g " 'org-agenda-show-and-scroll-up
    "gG" 'org-agenda-toggle-time-grid
    "gh" 'org-agenda-holidays
    "gj" 'org-agenda-goto-date
    "gJ" 'org-agenda-clock-goto
    "gk" 'org-agenda-action
    "gm" 'org-agenda-bulk-mark
    "go" 'org-agenda-open-link
    "gO" 'delete-other-windows
    "gr" 'org-agenda-redo
    "gv" 'org-agenda-view-mode-dispatch
    "gw" 'org-agenda-week-view
    "g/" 'org-agenda-filter-by-tag
  				;"h"  'org-agenda-earlier
    "b"  'org-agenda-earlier
    "i"  'org-agenda-diary-entry
    "j"  'org-agenda-next-line
    "k"  'org-agenda-previous-line
  				;"l"  'org-agenda-later
    "m" 'org-agenda-bulk-mark
  				;"n" nil                           ; evil-search-next
    "o" 'delete-other-windows
  				;p
    "q" 'org-agenda-quit
    "r" 'org-agenda-redo
    ;;"s" 'org-agenda-schedule ;;conflicts with avy
    "t" 'org-agenda-todo
    "u" 'org-agenda-bulk-unmark
  				;v
    "W" 'org-agenda-week-view
    "x" 'org-agenda-exit
    "y" 'org-agenda-year-view
    "z" 'org-agenda-add-note
    "{" 'org-agenda-manipulate-query-add-re
    "}" 'org-agenda-manipulate-query-subtract-re
    "$" 'org-agenda-archive
    "%" 'org-agenda-bulk-mark-regexp
    "+" 'org-agenda-priority-up
  				;"," 'org-agenda-priority
    "-" 'org-agenda-priority-down
    "." 'org-agenda-goto-today
    "0" 'evil-digit-argument-or-evil-beginning-of-line
    ":" 'org-agenda-set-tags
    ";" 'org-timer-set-timer
    "<" 'org-agenda-filter-by-category
    ">" 'org-agenda-date-prompt
    "?" 'org-agenda-show-the-flagging-note
    "A" 'org-agenda-append-agenda
    "B" 'org-agenda-bulk-action
    "C" 'org-agenda-convert-date
    ;;"D" 'org-agenda-toggle-diary
    "D" 'org-agenda-day-view
    "E" 'org-agenda-entry-text-mode
    "F" 'org-agenda-follow-mode
  				;G
    "H" 'org-agenda-holidays
    "I" 'org-agenda-clock-in
    "J" 'org-agenda-next-date-line
    "K" 'org-agenda-previous-date-line
    "L" 'org-agenda-recenter
    "M" 'org-agenda-phases-of-moon
  				;N
    "O" 'org-agenda-clock-out
    "P" 'org-agenda-show-priority
  				;Q
    "R" 'org-agenda-refile
    ;;"R" 'org-agenda-clockreport-mode ;; Maybe reassign if I learn what do
    ;;"S" 'org-save-all-org-buffers
    "S" 'org-agenda-schedule
    "T" 'org-agenda-show-tags
  				;U
  				;V
  				;W
    "X" 'org-agenda-clock-cancel
  				;Y
  				;Z
    "[" 'org-agenda-manipulate-query-add
    "g\\" 'org-agenda-filter-by-tag-refine
    "]" 'org-agenda-manipulate-query-subtract

    ;; prefix for my custom map
    "," 'agenda-mode-map-keys


    )

    (define-key leader-menu "o" 'org-global-hydra/body)
    (define-key leader-menu "M" 'mail-hydra/body)

    ;;;; org-capture setting
    ;; Start capture mode in evil insert state

    ;; Map for my custom ',' prefix in capture mode
    (define-prefix-command 'capture-mode-map-keys)
    (evil-define-key 'normal org-capture-mode-map "," 'capture-mode-map-keys)
    (define-key capture-mode-map-keys "k" 'org-capture-kill)
    (define-key capture-mode-map-keys "r" 'org-capture-refile)
    (define-key capture-mode-map-keys "c" 'org-capture-finalize)
    (define-key capture-mode-map-keys "t" 'org-set-tags-command)

  (define-prefix-command 'flycheck-keys)
  (define-key leader-menu "e" 'flycheck-keys)
  (define-key flycheck-keys "p" 'flycheck-previous-error)
  (define-key flycheck-keys "n" 'flycheck-next-error)

  )

(use-package which-key
  :straight t
  :diminish which-key-mode
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode)
  (setq which-key-idle-delay 0.3)
  (which-key-add-key-based-replacements
    "SPC SPC" "command"
    "SPC '" "prev buffer"
    "SPC a" "app"
    "SPC b" "buffer"
    "SPC e" "error"
    "SPC f" "file"
    "SPC i" "ivy"
    "SPC g" "git"
    "SPC o" "org"
    "SPC p" "projectile"
    "SPC s" "space"
    "SPC t" "toggle"
    "SPC w" "window"
    )

  )

(provide 'init-keys)
