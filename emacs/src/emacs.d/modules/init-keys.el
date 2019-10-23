(global-set-key (kbd "<escape>")      'keyboard-escape-quit)

(use-package hydra
  :straight t
  :config
  ;; currently unused
;  (defhydra hydra-window-size (:color blue
;			       :hint nil
;			       evil-normal-state-map "SPC w")
;    "
;  ^Mark^             ^Unmark^           ^Actions^          ^Search
;  ^^^^^^^^-----------------------------------------------------------------
;  _m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch
;  _s_: save          _U_: unmark up     _b_: bury          _I_: isearch
;  _d_: delete        ^ ^                _g_: refresh       _O_: multi-occur
;  _D_: delete up     ^ ^                _T_: files only: % -28`Buffer-menu-files-only
;  _~_: modified                         _e_: inc
;  "
;
;  ("e" evil-window-increase-width)
;  ("c" evil-window-decrease-width))
;
;
;  (defhydra hydra-buffer-menu (:color pink
;                               :hint nil)
;    "
;  ^Mark^             ^Unmark^           ^Actions^          ^Search
;  ^^^^^^^^-----------------------------------------------------------------
;  _m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch
;  _s_: save          _U_: unmark up     _b_: bury          _I_: isearch
;  _d_: delete        ^ ^                _g_: refresh       _O_: multi-occur
;  _D_: delete up     ^ ^                _T_: files only: % -28`Buffer-menu-files-only
;  _~_: modified
;  "
;  ("m" Buffer-menu-mark)
;  ("u" Buffer-menu-unmark)
;  ("U" Buffer-menu-backup-unmark)
;  ("d" Buffer-menu-delete)
;  ("D" Buffer-menu-delete-backwards)
;  ("s" Buffer-menu-save)
;  ("~" Buffer-menu-not-modified)
;  ("x" Buffer-menu-execute)
;  ("b" Buffer-menu-bury)
;  ("g" revert-buffer)
;  ("T" Buffer-menu-toggle-files-only)
;  ("O" Buffer-menu-multi-occur :color blue)
;  ("I" Buffer-menu-isearch-buffers :color blue)
;  ("R" Buffer-menu-isearch-buffers-regexp :color blue)
;  ("c" nil "cancel")
;  ("v" Buffer-menu-select "select" :color blue)
;  ("o" Buffer-menu-other-window "other-window" :color blue)
;  ("q" quit-window "quit" :color blue))

  ;(define-key Buffer-menu-mode-map "." 'hydra-buffer-menu/body)


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
  )


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
  (define-key leader-menu (kbd "SPC") 'counsel-M-x)
  (define-key leader-menu (kbd "'") 'switch-to-previous-buffer)
  (define-key leader-menu (kbd ",") 'evil-window-next)
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
                      "b" 'ivy-switch-buffer
                      "s" 'save-buffer
                      "f" 'counsel-find-file
                      "w" 'write-file
                      "r" 'counsel-recentf
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
         "s" 'avy-goto-char-2
         ")" 'dired-next-subdir
         "(" 'dired-prev-subdir)))

  (define-key leader-menu "s" 'persp-key-map)

  ;(define-key evil-normal-state-map "s" 'avy-goto-char-timer)
  ;(define-key evil-normal-state-map "s" 'avy-goto-char-flex)
  (define-key evil-normal-state-map "s" 'avy-goto-word-1)

  (define-prefix-command 'ivy-keys)
  (define-key leader-menu "i" 'ivy-keys)
  (define-key ivy-keys "i" 'counsel-imenu)
  (define-key ivy-keys "g" 'counsel-grep)

  (define-prefix-command 'magit-keys)
  (define-key leader-menu "g" 'magit-keys)
  (define-key magit-keys "s" 'magit-status)
  (define-key magit-keys "b" 'magit-blame)
  (define-key magit-keys "B" 'magit-blame-quit)

  (define-key toggle-keys "g" 'global-git-gutter-mode)

  (define-prefix-command 'projectile-keys)
  (define-key leader-menu "p" 'projectile-keys)
  (define-key projectile-keys "f" 'counsel-projectile-find-file)
  (define-key projectile-keys "b" 'counsel-projectile-switch-to-buffer)
  (define-key projectile-keys "p" 'counsel-projectile-switch-project)
  (define-key projectile-keys "a" 'counsel-projectile-ag)

  (define-prefix-command 'org-mode-keys)
  (evil-define-key 'normal org-mode-map (kbd ",") 'org-mode-keys)
  (define-key org-mode-keys "s" 'org-schedule)
  (define-key org-mode-keys "r" 'org-refile)
  (define-key org-mode-keys "t" 'counsel-org-tag)

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

    (define-prefix-command 'org-keys)
    (define-key leader-menu "o" 'org-keys)
    (define-key org-keys "c" 'org-capture)
    (define-key org-keys "g" (lambda () (interactive) (org-agenda nil " ")))
    (define-key org-keys "a" 'org-agenda)
    (define-key org-keys "l" 'org-agenda-list)
    (define-key org-keys "t" (lambda () (interactive) (org-capture nil "t")))
    (define-key org-keys "wt" (lambda () (interactive) (org-capture nil "w")))
    (define-key org-keys "wn" 'ftzm/capture-work-diary)
    (define-key org-keys "W" 'ftzm/org-agenda-list-work)
    (define-key org-keys "T" 'org-todo-list)
    (define-key org-keys "m" 'create-meeting-file)
    (define-key org-keys "sn" 'ftzm/search-notes)
    (define-key org-keys "d" 'ftzm/capture-diary)

    ;;;; org-capture setting
    ;; Start capture mode in evil insert state

    ;; Map for my custom ',' prefix in capture mode
    (define-prefix-command 'capture-mode-map-keys)
    (evil-define-key 'normal org-capture-mode-map "," 'capture-mode-map-keys)
    (define-key capture-mode-map-keys "k" 'org-capture-kill)
    (define-key capture-mode-map-keys "r" 'org-capture-refile)
    (define-key capture-mode-map-keys "c" 'org-capture-finalize)
    (define-key capture-mode-map-keys "t" 'counsel-org-tag)

  (define-prefix-command 'flycheck-keys)
  (define-key leader-menu "e" 'flycheck-keys)
  (define-key flycheck-keys "p" 'flycheck-previous-error)
  (define-key flycheck-keys "n" 'flycheck-next-error)

  )

(use-package which-key
  :straight t
  :diminish which-key-mode
  :config
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
