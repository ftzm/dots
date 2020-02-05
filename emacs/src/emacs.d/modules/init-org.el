;; org requires this hacky shit, this will install it on every run, should
;; optimize this. Got this recipe from the straight.el readme


; (require 'subr-x)
; (straight-use-package 'git)
;
; (defun org-git-version ()
;   "The Git version of org-mode.
; Inserted by installing org-mode or when a release is made."
;   (require 'git)
;   (let ((git-repo (expand-file-name
;                    "straight/repos/org/" user-emacs-directory)))
;     (string-trim
;      (git-run "describe"
;               "--match=release\*"
;               "--abbrev=6"
;               "HEAD"))))
;
; (defun org-release ()
;   "The release version of org-mode.
; Inserted by installing org-mode or when a release is made."
;   (require 'git)
;   (let ((git-repo (expand-file-name
;                    "straight/repos/org/" user-emacs-directory)))
;     (string-trim
;      (string-remove-prefix
;       "release_"
;       (git-run "describe"
;                "--match=release\*"
;                "--abbrev=0"
;                "HEAD")))))
;
; (provide 'org-version)

(use-package org
  ;:mode (("\\.org$" . org-mode))
  :straight (org org-plus-contrib)
  :config
  (add-hook 'org-mode-hook #'visual-line-mode)
  (progn
    (setq org-directory "~/org")
    (setq canonical-org-agenda-files (quote ("~/org/work.org"
				   "~/org/inbox.org"
				   "~/org/todo.org"
				   "~/org/projects/"
				   "~/org/main.org")))
    (setq org-agenda-files canonical-org-agenda-files)

    ;;There are other options for this that may deserve investigation
    (setq org-agenda-window-setup 'current-window)

    ;;handles hiding leading stars and indenting text
    (add-hook 'org-mode-hook 'org-indent-mode)

    (add-hook 'org-mode-hook 'flyspell-mode)

    (add-hook 'org-mode-hook 'olivetti-mode)

    ;; Autosave
    ;; passive saving
    (add-hook 'auto-save-hook (lambda () (let ((inhibit-message t)) (org-save-all-org-buffers))))
    ;; save on specific actions
    (advice-add 'org-agenda-quit :before (lambda () (let ((inhibit-message t)) (org-save-all-org-buffers))))


    ;; automatically save org buffers when agenda open
    ;;;(add-hook 'org-agenda-mode-hook
    ;;;      (lambda ()
    ;;;        (add-hook 'auto-save-hook 'org-save-all-org-buffers nil t)
    ;;;        (auto-save-mode)))
    ;;;

    ;; apply CLOSED property on done
    (setq org-log-done 'time)

    ;; enable log mode, which will show closed items in agenda view
    (setq org-agenda-start-with-log-mode t)

    ;; start capture in insert mode
    (add-hook 'org-capture-mode-hook 'evil-insert-state)

    (defun create-dated-file (path)
      (let ((name (read-string "Name: ")))
	(expand-file-name (format "%s-%s.org"
				  (format-time-string "%Y-%m-%d")
				  name) path)))

    (defun create-meeting-file ()
      (interactive)
      (find-file (replace-regexp-in-string " " "-" (create-dated-file "~/org/meetings")))
      )

  (defun agenda-remove-schedule ()
    (interactive)
    (org-agenda-schedule '(4))
    )


    ;; get tag completion from target file during capture with org-set-tag-function
    (add-hook 'org-capture-mode-hook
          (lambda ()
            (save-restriction
              (widen)
              (setq-local org-tag-alist (org-get-buffer-tags)))))

    (defun ftzm/capture-diary ()
      (interactive)
      (let ((title (read-string "Title: ")))
	(org-capture nil "d")))

    (defun ftzm/capture-work-diary ()
      (interactive)
      (let ((title (read-string "Title: "))
	    (tag "work:"))
	(org-capture nil "d")))

    (defun ftzm/var-defaulted (variable def-val)
      "Args: quoted var, quoted expression that returns string
      that returns string. Uses the value of the var if it has
      been defined, otherwise evals the expression and returns
      that."
      (if (boundp variable) (eval variable) (eval def-val)))


    (setq org-capture-templates
	  (quote (("t" "todo" entry (file "~/org/inbox.org")
		   "* NEXT %?")
		  ("d" "diary entry" entry (file+datetree "~/org/diary.org")
		   "* %(ftzm/var-defaulted 'title '(read-string \"Title\"))\
                      :diary:%(ftzm/var-defaulted 'tag '\"\") \n%?")
		  ("w" "work todo" entry (file+headline "~/org/work.org" "Tasks")
		   "* NEXT %?")
		  ("P" "process-soon" entry (file+headline "todo.org" "Todo")
		   "* TODO %a %?\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))")
		  )))


    (defun ftzm/search-diary ()
      (interactive)
      (let ((org-agenda-files '( "~/org/diary.org"))
	    (searchstring (read-string "Search: ")))
	(org-search-view nil (concat "" searchstring))))
	;(org-agenda-manipulate-query-gdd))

    (defun ftzm/org-agenda-list-work ()
      (interactive)
      (let ((org-agenda-tag-filter-preset '("work")))
    	(org-agenda-list)))


    ;; Exclude DONE state tasks from refile targets
    (defun bh/verify-refile-target ()
      "Exclude todo keywords with a done state from refile targets"
      (not (member (nth 2 (org-heading-components)) org-done-keywords)))

    (setq org-refile-target-verify-function 'bh/verify-refile-target)

    ;; allow refiling 9 levels deep
    (setq org-refile-targets '((nil :maxlevel . 9)
			       (org-agenda-files :maxlevel . 9)))

    ;; Refile in a single go
    (setq org-outline-path-complete-in-steps nil)

    ;; show full path for refiling, preceeded by the filename.
    (setq org-refile-use-outline-path 'file)
    ;; config stuff

    ;; Agenda Mode Settings
    (setq org-deadline-warning-days 7)


    (setq org-agenda-prefix-format '(
				     ;;(agenda  . " %i %-12:c%?-12t% s") ;; file name + org-agenda-entry-type
				     (agenda  . " %i %?-12t% s") ;; remove file name (And hopefully just that)
				     (timeline  . "  % s")
				     (todo  . " %i %-12:c")
				     (tags  . " %i %-12:c")
				     (search . " %i %-12:c")))

    ;; Custom sorting in agenda mode
    (setq org-agenda-sorting-strategy
    	  '((agenda time-up todo-state-down scheduled-up priority-down)
    	    ;; Original value of above:
    	    ;;(agenda habit-down time-up priority-down category-keep)
    	    (todo priority-down category-up)
    	    (tags priority-down category-keep)
    	    (search category-keep)))

    ;; VI keybinds for agenda mode stolen from abandonware "evil-rebellion"
    ;; Now has been customized a fair bit actually
    (evil-set-initial-state 'org-agenda-mode 'normal)


    (defun agenda-remove-schedule ()
      (interactive)
      (org-agenda-schedule '(4))
      )

    (setq org-todo-keywords
      '((sequence "TODO" "NEXT" "DONE")))

    ;; for GTD

    (setq org-agenda-block-separator nil)
    (setq ftzm/org-agenda-todo-view
      `(" " "Agenda"
        ((agenda ""
                 ((org-agenda-span 'day)
		  (org-agenda-skip-function  '(org-agenda-skip-entry-if 'todo 'done))
                 ))
         (todo "TODO|NEXT"
               ((org-agenda-overriding-header "To Refile")
                (org-agenda-files '("~/org/inbox.org"))))
	 ;; Exclude todo entries scheduled in the future. This way entries can
	 ;; be postponed by scheduling them, as a sort of integrated "tickler" function.
         (tags "TODO=\"TODO\"+SCHEDULED<=\"<today>\"|TODO=\"NEXT\""
               ((org-agenda-overriding-header "Single tasks")
                (org-agenda-files '("~/org/todo.org"))))
         (tags "TODO=\"TODO\"+SCHEDULED<=\"<today>\"|TODO=\"NEXT\""
               ((org-agenda-overriding-header "Work")
                (org-agenda-files '("~/org/work.org"))
	       ))
         (todo "NEXT"
               ((org-agenda-overriding-header "Projects")
                (org-agenda-files '("~/org/projects"))
	       ))
         (tags "CLOSED>=\"<today>\""
               ((org-agenda-overriding-header "Completed Tasks")
                (org-agenda-files canonical-org-agenda-files)
                ))
	 ;;(agenda ""
	 ;;	 ((org-agenda-overriding-header "Completed Tasks")
	 ;;	  (org-agenda-span 'day)
         ;;         (org-agenda-start-with-log-mode t)
	 ;;	  (org-agenda-use-time-grid nil)
         ;;         (org-agenda-skip-function
         ;;          '(org-agenda-skip-entry-if 'nottodo 'done))
         ;;        ))
         ;;(todo "TODO"
         ;;      ((org-agenda-overriding-header "Projects")
         ;;       (org-agenda-files '("~/.org/gtd/projects.org"))
         ;;       ))
         ;;(todo "TODO"
         ;;      ((org-agenda-overriding-header "One-off Tasks")
         ;;       (org-agenda-files '("~/.org/gtd/next.org"))
         ;;       (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
         nil)))

    (setq org-agenda-custom-commands
      `(;;,jethro/org-agenda-inbox-view
        ;;,jethro/org-agenda-someday-view
        ,ftzm/org-agenda-todo-view
        ;; ,jethro/org-agenda-papers-view ;; archived
        ))

    (defun ftzm/switch-to-agenda ()
    (interactive)
    (org-agenda nil " "))

    (bind-key "<f1>" 'ftzm/switch-to-agenda)

    )

;; ----------------------------------------------------------------------------
;; Custom Archive Function

  (defun days-ago (number)
    (time-subtract (current-time) (seconds-to-time (* number 86400)))
    )

  (defun archive-if-old ()
    (let*
	((props (org-entry-properties (point)))
         (closed-string (cdr (assoc "CLOSED" props)))
	 (title (cdr (assoc "ITEM" props)))
	 (closed (if closed-string (date-to-time closed-string) (days-ago 30)))
	 (cutoff (days-ago 6)))
         (when (time-less-p closed cutoff) ((lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading))
     (org-save-all-org-buffers))))
    ))

  (defun org-archive-done-tasks ()
    (interactive)
    (org-map-entries 'archive-if-old "/DONE" 'agenda))




  ;; stolen from here: https://emacs.stackexchange.com/questions/26351/custom-sorting-for-agenda
  (defun cmp-date-property (prop)
  "Compare two `org-mode' agenda entries, `A' and `B', by some date property.

If a is before b, return -1. If a is after b, return 1. If they
are equal return t."
  (lexical-let ((prop prop))
  #'(lambda (a b)

    (let* ((a-pos (get-text-property 0 'org-marker a))
           (b-pos (get-text-property 0 'org-marker b))
           (a-date (or (org-entry-get a-pos prop)
                       (format "<%s>" (org-read-date t nil "now"))))
           (b-date (or (org-entry-get b-pos prop)
                       (format "<%s>" (org-read-date t nil "now"))))
           (cmp (compare-strings a-date nil nil b-date nil nil))
           )
      (if (eq cmp t) nil (signum cmp))
      ))))

  ;; (setq org-agenda-custom-commands
  ;;     '(("h" "Agenda and Home-related tasks"
  ;;        ((agenda "")
  ;;         (tags-todo "home")
  ;;         (tags "garden")))
  ;;       ("o" "Agenda and Office-related tasks"
  ;;        ((agenda "")
  ;;         (tags-todo "work")
  ;;         (tags "office")))

  ;;       (" " "Agenda"
  ;;              ((tags "+TODO=\"TODO\"+REFILE"
  ;;                     ((org-agenda-overriding-header "Tasks to Refile")
  ;;                      (org-tags-match-list-sublevels nil)))

  ;; 		(tags "+TODO=\"DONE\"+CLOSED>\"<today>\""
  ;; 		      ((org-agenda-overriding-header "Done tasks")
  ;; 		       )
  ;; 		      )



  ;; 		)
  ;;         ((org-agenda-overriding-columns-format "%40ITEM %TODO %SCHEDULED %CLOSED")
  ;; 		       (org-agenda-view-columns-initially t))

  ;; 	       )


  ;; 	)

  ;;     )
  (defun org-todo-with-date (&optional arg)
  (interactive "P")
  (cl-letf* ((org-read-date-prefer-future nil)
             (my-current-time (org-read-date t t nil "when:" nil nil nil))
            ((symbol-function #'org-current-effective-time)
             #'(lambda () my-current-time)))
    (org-todo arg)
    ))

  (defun org-agenda-todo-with-date (&optional arg)
  (interactive "P")
  (cl-letf* ((org-read-date-prefer-future nil)
             (my-current-time (org-read-date t t nil "when:" nil nil nil))
            ((symbol-function #'org-current-effective-time)
             #'(lambda () my-current-time)))
    (org-agenda-todo arg)
    ))
  )

(use-package org-habit)

;; (use-package org-depend
  ;; :config
  ;; ;; This make it so that when a TODO item is moved from NEXT to DONE, the one
  ;; ;; below will automaticaly be set to NEXT.
  ;; (defun mm/org-insert-trigger ()
  ;; "Automatically insert chain-find-next trigger when entry becomes NEXT"
  ;; (cond ((equal org-state "NEXT")
         ;; (unless org-depend-doing-chain-find-next
           ;; (org-set-property "TRIGGER" "chain-find-next(NEXT,from-current,priority-up,effort-down)")))
        ;; ((not (member org-state org-done-keywords))
         ;; (org-delete-property "TRIGGER"))))
  ;; (add-hook 'org-after-todo-state-change-hook 'mm/org-insert-trigger)
  ;; )

; (use-package org-super-agenda
;   :straight t
;   :config
;   (org-super-agenda-mode 0)
;   (setq org-super-agenda-groups
;        '(;; Each group has an implicit boolean OR operator between its selectors.
;
; 	 ;; discard duplicate closed entries
; 	 (:discard (:and (:todo "DONE" :regexp "Scheduled")))
;
;          (:name "Closed"  ; Optionally specify section name
;                 ;:todo "DONE"  ; Items that have this TODO keyword
; 		:regexp "Closed"
; 		:order 99
; 		)
;
; 	 (:name "Habits"
; 		:habit t
; 		:order 15)
;
; 	 (:name "To Refile"
; 		:tag "REFILE"
; 		:order 1)
;
; 	 (:name "Work"
; 		:tag "work"
; 		:order 5)
;
; 	 (:name "Personal"
; 		:todo "TODO"
; 		:order 10)
;
; 	 (:discard (:anything t)) ;; discard final non-matching entries
;
; 	)
; 	)
;
;   )

(use-package org-indent
  :diminish org-indent-mode
  )

; (use-package calfw
;   :straight t
;   )
;
; (use-package calfw-org
;   :straight t
;   )

(use-package org-board
  :straight t
  )

(provide 'init-org)
