(defun keymap-symbol (keymap)
  "Return the symbol to which KEYMAP is bound, or nil if no such symbol exists."
  (catch 'gotit
    (mapatoms (lambda (sym)
                (and (boundp sym)
                     (eq (symbol-value sym) keymap)
                     (not (eq sym 'keymap))
                     (throw 'gotit sym))))))

;; in *scratch*:
;(keymap-symbol (current-local-map))

(global-set-key (kbd "<escape>")      'keyboard-escape-quit)

(use-package which-key
  :straight t
  :diminish which-key-mode
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode)
  (setq which-key-idle-delay 0.3))

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
  :after evil
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
  ;; (general-define-key
  ;;  :states '(normal visual motion)
  ;;  :keymaps '(override
  ;;             dired-mode-map
  ;;   	      magit-mode-map
  ;; 	      magit-blame-mode-map
  ;;   	      evil-normal-state-map)
  ;;  "SPC"
  ;;  'leader-menu)

  (general-evil-setup)

  ;;; Vim Mappings

  (general-nmap "c" ;; this must be defined after evil to bind c
              (general-key-dispatch 'evil-change
                "c" (general-simulate-key ('evil-change "e"))
                ))
  (general-vmap "c" 'evil-change)
  (general-nmap "s" 'avy-goto-char-flex)

  ;;; Leader Key Mappings

  (general-create-definer my-leader-def
    :prefix "SPC")

  ;; (which-key-add-key-based-replacements
  ;;   "SPC SPC" "command"
  ;;   "SPC '" "prev buffer"
  ;;   "SPC a" "app"
  ;;   "SPC b" "buffer"
  ;;   "SPC e" "error"
  ;;   "SPC f" "file"
  ;;   "SPC i" "ivy"
  ;;   "SPC g" "git"
  ;;   "SPC o" "org"
  ;;   "SPC p" "projectile"
  ;;   "SPC s" "space"
  ;;   "SPC t" "toggle"
  ;;   "SPC w" "window"
  ;; )

  (defun ftzm/flip-window ()
  (interactive)
  (let ((win  (get-mru-window nil nil t)))
    (when win (select-window win))))

  (my-leader-def
    :states '(normal visual)
    :keymaps 'override
    "SPC" '(execute-extended-command :which-key "command")
    "'" '(switch-to-previous-buffer :which-key "other buffer")
    "," '(ftzm/flip-window :which-key "previous window")
    "a" '(app-keys :which-key "apps")
    "b" '(buffer-keys :which-key "buffer")
    "b" '(buffer-keys :which-key "buffer")
    "e" '(flycheck-keys :which-key "error")
    "i" '(ivy-keys :which-key "ivy")
    "g" '(magit-keys :which-key "git")
    "o" '(org-global-hydra/body :which-key "org")
    "p" '(projectile-keys :which-key "project")
    "s" '(persp-hydra/body :which-key "workspace")
    "t" '(toggle-keys :which-key "toggle")
    "w" '(window-keys :which-key "window")
    "m" '(mpd-hydra/body :which-key "mpd")
    "M" '(mail-hydra/body :which-key "mail")
    "0" 'winum-select-window-0
    "1" 'winum-select-window-1
    "2" 'winum-select-window-2
    "3" 'winum-select-window-3
    "4" 'winum-select-window-4
    "5" 'winum-select-window-5
    "6" 'winum-select-window-6
    "7" 'winum-select-window-7
    "8" 'winum-select-window-8
    "9" 'winum-select-window-9
   )

  (general-define-key
   :keymaps '(override vertico-map)
   "M-0" 'winum-select-window-0
   "M-1" 'winum-select-window-1
   "M-2" 'winum-select-window-2
   "M-3" 'winum-select-window-3
   "M-4" 'winum-select-window-4
   "M-5" 'winum-select-window-5
   "M-6" 'winum-select-window-6
   "M-7" 'winum-select-window-7
   "M-8" 'winum-select-window-8
   "M-9" 'winum-select-window-9
   )

  (general-define-key
   :prefix-command 'window-keys
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

  (general-define-key
   :prefix-command 'buffer-keys
   "d" 'kill-this-buffer
   "e" 'eval-buffer
   "k" 'evil-prev-buffer
   "j" 'evil-next-buffer
   "b" 'switch-to-buffer
   "B" 'switch-to-buffer-other-window
   "s" 'save-buffer
   "f" 'find-file
   "w" 'write-file
   "r" 'consult-recent-file
   "u" 'sudo-find-file
   "U" 'sudo-this-file)

  (general-define-key
   :prefix-command 'toggle-keys
   "l" 'global-display-line-numbers-mode
   "g" 'global-git-gutter-mode
   "b" 'default-text-scale-increase
   "s" 'default-text-scale-decrease)

  (general-define-key
   :prefix-command 'app-keys
   "d" 'dired)

  (general-define-key
   :prefix-command 'magit-keys
   "s" 'magit-status
   "b" 'magit-blame
   "B" 'magit-blame-quit)

  (general-define-key
   :prefix-command 'ivy-keys
   "i" 'consult-imenu
   "g" 'consult-line)

  (general-define-key
   :prefix-command 'projectile-keys
   "f" 'projectile-find-file
   "F" 'projectile-find-file-other-window
   "b" 'projectile-switch-to-buffer
   "p" 'switch-persp-project
   "P" 'projectile-switch-project
   "a" 'consult-git-grep
   "t" 'projectile-run-vterm)

  (general-define-key
   :prefix-command 'flycheck-keys
   "p" 'evil-flycheck-previous
   "n" 'evil-flycheck-next)

  ;;; mode keys

  ;; (eval-after-load 'dired
  ;;   '(progn
  ;;      (evil-define-key 'normal dired-mode-map
  ;;        "G" 'evil-goto-line
  ;;        "gg" 'evil-goto-first-line
  ;;        "K" 'dired-kill-subdir
  ;;        "S" 'dired-sort-toggle-or-edit
  ;;        "s" 'avy-goto-char-flex
  ;;        ")" 'dired-next-subdir
  ;;        "(" 'dired-prev-subdir)))

  (general-define-key
   :states 'normal
   :keymaps 'org-mode-map
   :prefix ","
   "s" 'org-schedule
   "r" 'org-refile
   "t" 'org-set-tags-command)

  (general-define-key
   :states 'normal
   :keymaps 'org-capture-mode-map
   :prefix ","
   "k" 'org-capture-kill
   "r" 'org-capture-refile
   "c" 'org-capture-finalize
   "t" 'org-set-tags-command)

  ;; ;; Map for my custom ',' prefix.
  ;; (define-prefix-command 'agenda-mode-map-keys)
  ;; (general-define-key
  ;;  :keymaps 'agenda-mode-map)
  ;; (define-key agenda-mode-map-keys "w" 'org-agenda-week-view)
  ;; (define-key agenda-mode-map-keys "D" 'org-agenda-day-view)
  ;; (define-key agenda-mode-map-keys "d" 'org-agenda-deadline)
  ;; (define-key agenda-mode-map-keys "r" 'org-agenda-refile)
  ;; (define-key agenda-mode-map-keys "s" 'org-agenda-schedule)
  ;; (define-key agenda-mode-map-keys "p" 'org-agenda-priority)
  ;; (define-key agenda-mode-map-keys "f" 'org-agenda-filter-by-tag)
  ;; (define-key agenda-mode-map-keys "cs" 'agenda-remove-schedule)
  ;; (define-key agenda-mode-map-keys "k" 'org-agenda-kill)

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
    )

    ;;;; org-capture setting
    ;; Start capture mode in evil insert state

  (general-define-key
   :states 'normal
   :keymaps 'treemacs-mode-map
   "TAB" 'treemacs-TAB-action
   "RET" 'treemacs-RET-action
   "q" 'kill-buffer-and-window)




    (provide 'init-keys))
