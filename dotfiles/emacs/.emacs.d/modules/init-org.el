;; org requires this hacky shit, this will install it on every run, should
;; optimize this. Got this recipe from the straight.el readme

 ;; (require 'subr-x)
 ;; (straight-use-package 'git)

 ;; (defun org-git-version ()
 ;;   "The Git version of org-mode.
 ;; Inserted by installing org-mode or when a release is made."
 ;;   (require 'git)
 ;;   (let ((git-repo (expand-file-name
 ;;                    "straight/repos/org/" user-emacs-directory)))
 ;;     (string-trim
 ;;      (git-run "describe"
 ;;               "--match=release\*"
 ;;               "--abbrev=6"
 ;;               "HEAD"))))

 ;; (defun org-release ()
 ;;   "The release version of org-mode.
 ;; Inserted by installing org-mode or when a release is made."
 ;;   (require 'git)
 ;;   (let ((git-repo (expand-file-name
 ;;                    "straight/repos/org/" user-emacs-directory)))
 ;;     (string-trim
 ;;      (string-remove-prefix
 ;;       "release_"
 ;;       (git-run "describe"
 ;;                "--match=release\*"
 ;;                "--abbrev=0"
 ;;                "HEAD")))))

 ;; (provide 'org-version)

; (use-package load-theme-buffer-local
; :straight t)

;(use-package org-bullets
  ;:straight t
  ;:config
  ;(setq org-bullets-bullet-list '(" ")))

  ;(setq org-bullets-bullet-list '(" ")))

(use-package org-ql
  :straight t
  :config
  (defun zdo/org-ql-view--format-element (orig-fun &rest args)
    "This function will intercept the original function and
   add the category to the result.

   ARGS is `element' in `org-ql-view--format-element'"
    (if (not args)
        ""
      (let* ((element args)
             (properties (cadar element))
             (result (apply orig-fun element))
             (category (org-entry-get (plist-get properties :org-marker) "CATEGORY")))
        (org-add-props
            (format "   %-8s %s" (concat category ":") result)
            (text-properties-at 0 result)))))
  (advice-add 'org-ql-view--format-element :around #'zdo/org-ql-view--format-element)
  )

(use-package org
  :straight (:type built-in)
  :mode (("\\.org$" . org-mode))
  :commands (org-agenda ftzm/org-capture-frame)
   :hook (
  	 (org-mode . auto-revert-mode)
  	 (org-mode . yas-minor-mode)
  	 (org-mode . olivetti-mode)
  	 (org-mode . typo-mode)
   	 )
  :init
  (pretty-hydra-define org-global-hydra
    (:color blue :quit-key "q" :title "Org Dispatch")
    ("Agenda"
     (( "g" (org-agenda nil " ") "Personal Agenda")
      ( "wg" (org-agenda nil "W") "Work Agenda"))
     "Capture"
     (("t" (org-capture nil "t") "Task")
      ("d" (org-capture nil "d") "Diary")
      ("wt" (org-capture nil "w") "Work Task")
      ("wd" (org-capture nil "u") "Work Diary"))
      ))

  :config

  (setq org-ellipsis "...")

  ;(require 'org-num)
  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'org-mode-hook #'(lambda () (setq evil-auto-indent nil)))
  (progn
    (setq org-directory "~/org/personal/")
    (setq canonical-org-agenda-files (quote ("inbox.org"
					     "todo.org"
					     "~/org/personal/projects"
					     "special-dates.org"
					     "habits.org"
					     )))
    (setq org-agenda-files canonical-org-agenda-files)

    ;;There are other options for this that may deserve investigation
    (setq org-agenda-window-setup 'current-window)

    ;;handles hiding leading stars and indenting text
    ;(add-hook 'org-mode-hook 'org-indent-mode)
    ;(setq org-hide-leading-stars t)

    (setq org-hide-emphasis-markers nil)

    ;(defun my/org-mode-hook ()
    ;  "Stop the org-level headers from increasing in height relative to the other text."
    ;  (dolist (face '(org-level-2
    ;                  org-level-3
    ;                  org-level-4
    ;                  org-level-5
    ;                  org-level-6
    ;                  org-level-7
    ;                  org-level-8))
    ;    (set-face-attribute face nil :inherit outline-1)))

    ;(my/org-mode-hook)

    (setq org-num-face font-lock-comment-face)

    ; the languages that can be executed in org code blocks
    (setq org-plantuml-jar-path "/nix/store/zbr9v2vmp7456a0vkd8kq8s4s97fbci5-plantuml-1.2020.16/lib/plantuml.jar")
    (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t) (dot . t) (emacs-lisp . t)))
    (setq org-confirm-babel-evaluate nil)

    (setq org-display-inline-images t)
    (setq org-redisplay-inline-images t)
    (setq org-startup-with-inline-images t)

    ;; Autosave
    ;; passive saving
    ;;(add-hook 'auto-save-hook (lambda () (let ((inhibit-message t)) (org-save-all-org-buffers))))
    ;; save on specific actions
    (advice-add 'org-agenda-quit :before (lambda () (let ((inhibit-message t)) (org-save-all-org-buffers))))

    ;; apply CLOSED property on done
    (setq org-log-done 'time)

    ;; enable log mode, which will show closed items in agenda view
    (setq org-agenda-start-with-log-mode t)

    (setq org-adapt-indentation nil)
    ;; start capture in insert mode
    (add-hook 'org-capture-mode-hook 'evil-insert-state)

    (defun create-dated-file (path)
      (let ((name (read-string "Name: ")))
	(expand-file-name (format "%s-%s.org"
				  (format-time-string "%Y-%m-%d")
				  name) path)))

    (defun create-meeting-file ()
      (interactive)
      (find-file (replace-regexp-in-string " " "-" (create-dated-file "~/org/work/meeting")))
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

    (defun get-dir-org-tags (dir)
      (string-lines
       (shell-command-to-string
	(format "grep -roh ':[a-z]\\+:' %s/* | sort | uniq | tr -d :" dir))))

    (defun ftzm/org-set-tags-command ()
      (interactive)
      (save-excursion
	(org-back-to-heading)
	(let* ((global-tags (mapcar #'list (get-dir-org-tags "~/org")))
	       (all-tags (org-get-tags))
	       (current-tags
		(cl-remove-if
		 (lambda (tag) (get-text-property 0 'inherited tag)) all-tags))
	       (crm-separator "[ \t]*:[ \t]*")
	       (tags (mapconcat #'identity
                                (completing-read-multiple
			         "Tags: "
			         global-tags
			         nil nil (org-make-tag-string current-tags)
			         'org-tags-history)
                                ":")))
	  (org-set-tags tags))))


    (defun ftzm/var-defaulted (variable def-val)
      "Args: quoted var, quoted expression that returns string
      that returns string. Uses the value of the var if it has
      been defined, otherwise evals the expression and returns
      that."
      (if (boundp variable) (eval variable) (eval def-val)))


    (setq org-capture-templates
	  (quote (("t" "todo" entry (file "~/org/personal/inbox.org")
		   "* NEXT %?")
		  ("d" "diary entry" entry (file+datetree "~/org/personal/diary.org")
		   "* %?")
		  ("u" "work diary entry" entry (file+datetree "~/org/work/diary.org")
		   "* %?")
		  ("w" "work todo" entry (file "~/org/work/work-inbox.org")
		   "* NEXT %?")
		  ("m" "meeting" entry (file (lambda () (if (boundp 'meeting-title)
								      (format "~/org/work/meeting/%s-%s.org" (format-time-string "%Y-%m-%d")
									      (replace-regexp-in-string " " "-" meeting-title)) "~/org/work/meeting/meetings.org")))
		   (function (lambda () (format  "* NEXT %s %%?" (ftzm/var-defaulted 'meeting-title "meeting: ")))))
		  ("P" "process-soon" entry (file+headline "todo.org" "Todo")
		   "* TODO %a %?\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))")
		  )))

    (defun capture-meeting ()
      (interactive)
      (let ((meeting-title "worked thing"))
	(org-capture nil "m")))

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

    (setq org-use-fast-todo-selection t) ; select todo via key rather than cycling
    (setq org-use-fast-todo-selection 'expert) ; show options in minibuffer
    (setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "DONE(d)")))

    (defun org-sep-header (text)
      (let ((buffer-read-only nil))
	(insert
	 (propertize
	  text
	  'face (list :foreground (face-attribute
				   'font-lock-function-name-face :foreground) :weight 'bold)))))

    ;; for GTD

    (set 'org-habit-show-all-today t)

    (setq personal-agenda-items
	  (list
	   ))


    (setq org-super-agenda-header-separator "")

    (defun custom-agenda ()
      (interactive)
      (let ((org-super-agenda-groups
       '((:log t)  ; Automatically named "Log"
         (:name "Schedule"
                :time-grid t)
         (:name "Today"
                :scheduled today)
         (:habit t)
         (:name "Due today"
                :deadline today)
         (:name "Overdue"
                :deadline past)
         (:name "Due soon"
                :deadline future)
         (:name "Unimportant"
                :todo ("SOMEDAY" "MAYBE" "CHECK" "TO-READ" "TO-WATCH")
                :order 100)
         (:name "Waiting..."
                :todo "WAITING"
                :order 98)
         (:name "Scheduled earlier"
                :scheduled past))))
	(org-agenda nil "Q")))

    (setq org-agenda-block-separator nil)
    (setq ftzm/org-agenda-todo-view-ql-combined
	  `("Q" "test ql"
	    (
	     (org-ql-block '(or
			     ;; deadlined
			     (and (not (todo "DONE"))
				      (deadline auto))
			     ;; today
			     (and (not (todo "DONE"))
				 (scheduled :from today)
				 (not (category "habits")))
			     ;; habits
	 		     ;; (org-agenda-files '("~/org/habits.org"))
			     (habit)
			     ;; Inbox
			     (and (todo "TODO" "NEXT")
				 (not (scheduled))
				 (not (deadline))
				 (path "org/inbox.org"))
			     ;; Ad Hoc Shortlist
			     (and (todo "NEXT")
				 (not (scheduled))
				 (not (deadline))
				 (path "org/todo.org"))
			     ;; Project shortlist
			     (and (todo "NEXT")
				 (not (scheduled))
				 (not (deadline))
				 (path "org/projects")
				 )
			     ;; Completed
			     (and (closed :on today))
			     )
			  ((org-ql-block-header "Agenda")) ))))
    (setq ftzm/org-agenda-todo-view-ql
	  `("q" "test ql"
	    (
	     (org-ql-block '(and (not (todo "DONE"))
				 (deadline auto))
                        ((org-ql-block-header "Deadlines")))
	     (org-ql-block '(and (not (todo "DONE"))
	     			 (scheduled :from today)
	     			 (not (category "habits")))
                        ((org-ql-block-header "Today")))
	     (agenda ""
                     ((org-agenda-overriding-header "Habits")
	     	      (org-agenda-span 'day)
	     	      (org-agenda-files '("~/org/habits.org"))
                      ))
	     (org-ql-block '(and (todo "TODO" "NEXT")
	     			 (not (scheduled))
	     			 (not (deadline))
	     			 (path "org/inbox.org"))
                           ((org-ql-block-header "Inbox")))
	     (org-ql-block '(and (todo "NEXT")
	     			 (not (scheduled))
	     			 (not (deadline))
	     			 (path "org/todo.org"))
                           ((org-ql-block-header "Ad Hoc Shortlist yadig")
			    (org-agenda-prefix-format " ohfuckwhat %?-12t% s")))
	     (org-ql-block '(and (todo "NEXT")
	     			 (not (scheduled))
	     			 (not (deadline))
	     			 (path "org/projects"))
	     ;; 		    (org-ql-block-header "Project Shortlist")
	     ;; 		    (org-super-agenda-groups '(
	     ;; 		    			       (:auto-category t
	     ;; 		    			       )))))
	     ;; 		    ;; (org-super-agenda-groups '(
	     ;; 		    ;; 			       (:name "test"
	     ;; 		    ;; 				:anything t
	     ;; 		    ;; 				:transformer (--> it
	     ;; 		    ;; 						  (upcase it)
	     ;; 		    ;; 						  (propertize it 'face '(:foreground "RosyBrown1"))))
	     ;; 		    ;; 			       ))))
	     ;; (org-ql-block '(and (closed :on today))
             ;;               ((org-ql-block-header "Completed")))
	 ))))

    (setq ftzm/org-agenda-todo-view
      `(" " "Agenda"
        ((org-sep-header "Ní dhéanfaidh smaoineamh an treabhadh dhuit\n")
	 (agenda ""
                 ((org-agenda-overriding-header "Deadlines")
		  (org-agenda-span 'day)
		  (org-agenda-entry-types '(:deadline))
		  (org-agenda-skip-function  '(org-agenda-skip-entry-if 'todo 'done))
                 ))
	 (tags "TODO=\"NEXT\"+SCHEDULED<=\"<today>\"|TODO=\"TODO\"+SCHEDULED<=\"<today>\"-CATEGORY=\"habits\""
	       ((org-agenda-overriding-header "Today")))
	 (agenda ""
                 ((org-agenda-overriding-header "Habits")
	 	  (org-agenda-span 'day)
	 	  (org-agenda-files '("habits.org"))
                 ))
         (todo "TODO|NEXT"
               ((org-agenda-overriding-header "To Refile")
                (org-agenda-files '("inbox.org"))
	 	(org-agenda-todo-ignore-scheduled 'all)
	 	(org-agenda-todo-ignore-deadlines 'all)
	 	(org-agenda-prefix-format "  %?-12t% s")))
         (todo "NEXT"
               ((org-agenda-overriding-header "Ad Hoc Shortlist")
                (org-agenda-files '("todo.org"))
	 	(org-agenda-todo-ignore-scheduled 'all)
	 	(org-agenda-prefix-format "  %?-12t% s")
	 	))
         (todo "NEXT"
               ((org-agenda-overriding-header "Project Shortlist")
	 	(org-agenda-todo-ignore-scheduled 'all)
                (org-agenda-files '("~/org/personal/projects"))
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
	 )))

    (defun testf ()
      "oi")

    (setq ftzm/org-agenda-work-view
	  `("W" "Agenda"
            ((insert
	      (propertize
	       "Ní dhéanfaidh smaoineamh an treabhadh dhuit"
	       'face (list :foreground (face-attribute 'font-lock-function-name-face :foreground) :weight 'bold) ))
	     (agenda ""
                     ((org-agenda-span 'day)
					;(org-agenda-skip-function  '(org-agenda-skip-entry-if 'todo 'done))
		      (org-agenda-skip-function  '(org-agenda-skip-entry-if 'todo 'done))
		      (org-agenda-files '("~/org/work/"))
		      (org-agenda-overriding-header "\n"
						    )
                      ))
             (todo "TODO|NEXT"
		   ((org-agenda-overriding-header "To Refile")
                    (org-agenda-prefix-format "%c %i %?-12t% s")
		    (org-agenda-files '("~/org/work/work-inbox.org"))
		    ))
	     ;; Exclude todo entries scheduled in the future. This way entries can
	     ;; be postponed by scheduling them, as a sort of integrated "tickler" function.
             (tags "TODO=\"TODO\"+SCHEDULED<=\"<today>\"|TODO=\"NEXT\""
		   ((org-agenda-overriding-header "Next Up")
                    (org-agenda-files '("~/org/work/work-todo.org"))))
             (tags "CLOSED>=\"<today>\""
		   ((org-agenda-overriding-header "Completed Tasks")
                    (org-agenda-files '("~/org/work/"))
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
             nil
	     ("~/test-gtd.html"))))

    (setq org-agenda-custom-commands
      `(;;,jethro/org-agenda-inbox-view
        ;;,jethro/org-agenda-someday-view
	,ftzm/org-agenda-todo-view-ql
	,ftzm/org-agenda-todo-view-ql-combined
        ,ftzm/org-agenda-todo-view
        ,ftzm/org-agenda-work-view
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
               ((symbol-function 'current-time)
                #'(lambda () my-current-time))
               ((symbol-function 'org-today)
                #'(lambda () (time-to-days my-current-time)))
               ((symbol-function 'org-current-effective-time)
                #'(lambda () my-current-time))

               (super-org-entry-put (symbol-function 'org-entry-put))
               ((symbol-function 'org-entry-put)
                #'(lambda (pom property value)
                    (print property)
                    (if (equal property "LAST_REPEAT")
                        (let ((my-value (format-time-string (org-time-stamp-format t t) my-current-time)))
                          (funcall super-org-entry-put pom property my-value))
                      (funcall super-org-entry-put pom property value)
                      ))))
      (if (eq major-mode 'org-agenda-mode) (org-agenda-todo arg) (org-todo arg))))

(defun ftzm/delete-capture-frame (&rest _)
  "Delete frame when its name frame-parameter is set to \"capture\"."
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))
(advice-add 'org-capture-finalize :after #'ftzm/delete-capture-frame)

(defun ftzm/org-capture-frame (org-arg)
  "Run org-capture in its own frame."
  (interactive)
  (require 'cl-lib)
  (select-frame-by-name "capture")
  (delete-other-windows)
  (cl-letf (((symbol-function 'switch-to-buffer-other-window) #'switch-to-buffer))
    (condition-case err
        (progn  (ftzm/revert-all-file-buffers)
		(org-capture nil org-arg)
		(setq mode-line-format nil))
      ;; "q" signals (error "Abort") in `org-capture'
      ;; delete the newly created frame in this scenario.
      (user-error (when (string= (cadr err) "Abort")
                    (delete-frame))))))




(use-package org-habit
  :after org
  ))

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
  :after org
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
  :after org
  )

(use-package org-modern
  :straight t
  ;;   :after org
    :config
  ;;   ;; Choose some fonts
  ;;   ;; (set-face-attribute 'default nil :family "Iosevka")
  ;;   ;; (set-face-attribute 'variable-pitch nil :family "Iosevka Aile")
  ;;   ;; (set-face-attribute 'org-modern-symbol nil :family "Iosevka")

  ;;   ;; Add frame borders and window dividers
  ;;   ;; (modify-all-frames-parameters
  ;;   ;;  '((right-divider-width . 40)
  ;;   ;;    (internal-border-width . 40)))
  ;;   ;; (dolist (face '(window-divider
  ;;   ;;                 window-divider-first-pixel
  ;;   ;;                 window-divider-last-pixel))
  ;;   ;;   (face-spec-reset-face face)
  ;;   ;;   (set-face-foreground face (face-attribute 'default :background)))
  ;;   ;; (set-face-background 'fringe (face-attribute 'default :background))

  ;;   ;; (setq
  ;;   ;;  ;; Edit settings
  ;;   ;;  ;org-auto-align-tags nil
  ;;   ;;  ;org-tags-column 0
  ;;   ;;  org-catch-invisible-edits 'show-and-error
  ;;   ;;  org-special-ctrl-a/e t
  ;;   ;;  org-insert-heading-respect-content t

  ;;   ;;  ;; Org styling, hide markup etc.
  ;;   ;;  org-hide-emphasis-markers t
  ;;   ;;  org-pretty-entities t
  ;;   ;;  org-ellipsis "…"

  ;;   ;;  ;; Agenda styling
  ;;   ;;  ;org-agenda-tags-column 0
  ;;   ;;  ;org-agenda-block-separator ?─
  ;;   ;;  org-agenda-time-grid
  ;;   ;;  '((daily today require-timed)
  ;;   ;;    (800 1000 1200 1400 1600 1800 2000)
  ;;   ;;    " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
  ;;   ;;  org-agenda-current-time-string
  ;;   ;;  "<- now ─────────────────────────────────────────────────")

  ;;   ;; (set-face-attribute 'org-modern-done nil
  ;;   ;; 		      :foreground "#ebdbb2")
     (global-org-modern-mode)
  )

(provide 'init-org)
