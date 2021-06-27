(use-package ag
  :straight t)

;(use-package ivy
;  :straight t
;  :diminish ivy-mode
;  :config
;  (ivy-mode 1)
;  (setq ivy-count-format "%d/%d - ")
;  (setq ivy-height 15)
;  (setq ivy-use-virtual-buffers nil) ;; show recentf in switch-buffers --
;
;  ;;breaks prettify
;  (setq enable-recursive-minibuffers t)
;
;  (setq counsel-find-file-ignore-regexp
;        (concat
;         ;; File names beginning with # or .
;         "\\(?:\\`[#.]\\)"
;         ;; File names ending with # or ~
;         "\\|\\(?:\\`.+?[#~]\\'\\)"))
;
;
;  ;alternate cycle keys, more vimy
;  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
;  (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
;  (define-key ivy-switch-buffer-map (kbd "C-k") 'ivy-previous-line)
;
;  ;; below works but slow
;  (defconst modi/ag-arguments
;  '("--nogroup" ; mandatory argument for ag.el as per https://github.com/Wilfred/ag.el/issues/41
;      ;"--skip-vcs-ignores" ; Ignore files/dirs ONLY from `.agignore'
;      "--numbers" ; line numbers
;      "--smart-case"
;      "--follow") ; follow symlinks
;  "Default ag arguments used in the functions in `ag', `counsel' and `projectile'
;      packages.")
;
;  ;; Use `ag' all the time if available
;  (defun modi/advice-projectile-use-ag (&optional idontknowwhythisispassedsometimes)
;    "Always use `ag' for getting a list of all files in the project."
;    (mapconcat 'identity
;           (append '("\\ag") ; used unaliased version of `ag': \ag
;               modi/ag-arguments
;               '("-0" ; output null separated results
;             "-g ''")) ; get file names matching the regex '' (all files)
;           " "))
;  (when (executable-find "ag")
;    (advice-add 'projectile-get-ext-command :override
;        #'modi/advice-projectile-use-ag))
;
;  (with-eval-after-load 'org-agenda
;    (define-key org-agenda-mode-map (kbd "C-c C-q") #'counsel-org-tag-agenda))
;
;  ;; Prettify switch-buffer, from Doom
;  ;; anything other than buffers fed into this breaks it
;  (defun +ivy-buffer-transformer (str)
;  (let* ((buf (get-buffer str))
;         (path (buffer-file-name buf))
;         (mode (buffer-local-value 'major-mode buf))
;         (faces
;          (with-current-buffer buf
;            (cond ((string-match-p "^ ?\\*" (buffer-name buf))
;                   'font-lock-comment-face)
;                  ((buffer-modified-p buf)
;                   'font-lock-keyword-face)
;                  (buffer-read-only
;                   'error)))))
;    (propertize
;     (format "%-40s %-20s %s"
;             str
;             mode
;             (or (and path (abbreviate-file-name (file-name-directory (file-truename path))))
;                 ""))
;      'face faces)))
;
;  (ivy-set-display-transformer #'ivy-switch-buffer #'+ivy-buffer-transformer)
;  (ivy-set-display-transformer #'ivy-switch-buffer-other-window #'+ivy-buffer-transformer)
;  (ivy-set-display-transformer #'counsel-recentf #'abbreviate-file-name)
;
;  )

;;; enhances ivy-M-x, reorders function candidates
;(use-package smex
  ;:straight t
  ;)
;
;(use-package counsel
  ;:straight t
  ;)

;(use-package ivy-rich
;  :straight t
;  :config
;  (ivy-rich-mode 1)
;  )


(use-package consult
  :straight (consult
	     :branch "main")
  :config
  (setq completion-in-region-function #'consult-completion-in-region)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function 'projectile-project-root)
  (setq consult-async-split-style 'space)
  (setq consult-ripgrep-command "rg --null --line-buffered --color=ansi --max-columns=1000 -S --no-heading --line-number . -e ARG OPTS")
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-file consult--source-project-file consult--source-bookmark
   :preview-key (kbd "M-."))

  (defun grep-no-filename-pred (candidate)
    (let* ((car (car-safe candidate))
	   (haystack (if (and car (stringp car))
			 (replace-regexp-in-string "\\(^.*?:.*?:\\)" "" car)
		       nil)))
      (if haystack
	  (seq-every-p (lambda (reg) (string-match reg haystack))
		       completion-regexp-list)
	t)))

  (defun ftzm/consult-ripgrep (&optional dir initial)
    "Do a rg search which ignores the file name when filtering candidates."
    (interactive "P")
    (let ((minibuffer-completion-predicate 'grep-no-filename-pred)
	  (orderless-all-completions-orig (symbol-function 'orderless-all-completions)))
      (cl-letf (((symbol-function 'orderless--highlight)
     		 (lambda (regexps string)
		   ;; the definition is the same as the original but for
		   ;; the addition and use of the `start` variable.
		   (cl-loop with n = (length orderless-match-faces)
			    with start = (if (string-match "\\(^.*?:.*?:\\)" string)
     					     (match-end 0) 0)
			    for regexp in regexps and i from 0
			    when (string-match regexp string start) do
			    (cl-loop
			     for (x y) on (or (cddr (match-data)) (match-data)) by #'cddr
			     when x do
			     (add-face-text-property
			      x y
			      (aref orderless-match-faces (mod i n))
			      nil string)))
		   string))
		((symbol-function 'orderless-all-completions)
		 (lambda (string table _pred _point)
		   (funcall orderless-all-completions-orig string table 'grep-no-filename-pred _point))))
	(consult-ripgrep dir initial))))
  )
;  :straight (multi-libvterm
;	     :type git
;	     :host github
;	     :repo "suonlight/multi-libvterm"))

(use-package embark
  :straight t)

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  :straight (marginalia :branch "main")
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package orderless
  :straight t
  :custom (completion-styles '(orderless)))

(defun grep-no-filename-pred2 (x)
  ;(with-current-buffer "*scratch*" (insert (format "input: %s" x)))
  ;; (if (not (listp x))
  ;; (with-current-buffer "*scratch*" (insert "\n"))
  ;; (with-current-buffer "*scratch*" (insert (format "%s" (car-safe x))))
  ;; (with-current-buffer "*scratch*" (insert "\n"))
  ;; (with-current-buffer "*scratch*" (insert (format "%s" (car-safe x))))
  ;; (with-current-buffer "*scratch*" (insert "\n"))
  ;; (with-current-buffer "*scratch*" (insert (format "%s" (hash-table-p x))))
  ;; (with-current-buffer "*scratch*" (insert "\n"))
  ;;   (with-current-buffer "*scratch*" (insert (format "weird input: %s" x)))
  ;;     )
  ;; (if (consp x)
  ;; (with-current-buffer "*scratch*" (insert "\n"))
  ;; (with-current-buffer "*scratch*" (insert "cons"))
  ;; (with-current-buffer "*scratch*" (insert "\n"))
  ;; ;(with-current-buffer "*scratch*" (insert (format "%s" (car x))))
  ;;     )
  ;; (with-current-buffer "*scratch*" (insert "\n"))
  ;; (with-current-buffer "*scratch*" (insert "-------------"))
  ;; (with-current-buffer "*scratch*" (insert "\n"))
  (let* ((carO (car-safe x))
	 (search-space (if (and carO (stringp carO))
			   (replace-regexp-in-string "\\(^.*?:\\)" "" (format "%s" carO))
			 nil)))
    ;(with-current-buffer "*scratch*" (insert "\n"))
    ;(with-current-buffer "*scratch*" (insert (format "search space: %s" search-space)))

  (if search-space

      (let ((success
	     (-all? (lambda (reg)
		      (string-match reg search-space))
		    completion-regexp-list)))

	success)
    t)))
  ;; (if (car-safe x)
  ;;     (
  ;; (with-current-buffer "*scratch*" (insert "\n"))
  ;; (with-current-buffer "*scratch*" (insert "hit meat"))
  ;; (with-current-buffer "*scratch*" (insert "\n"))
  ;; ;(with-current-buffer "*scratch*" (insert (format "input: %s" x)))
  ;; (let ((search-space (replace-regexp-in-string "\\(^.*?:\\)" "" (format "%s" (car-safe
  ;; 									       x)))))
  ;; ;(with-current-buffer "*scratch*" (insert search-space))
  ;;     (-all? (lambda (reg)
  ;; 	       (string-match reg ))
  ;; 	     completion-regexp-list))
  ;;   )
  ;;      t)))

(defun orderless-all-completions-ignore2 (string table _pred _point)
  ;(with-current-buffer "*scratch*" (erase-buffer))
  ;(with-current-buffer "*scratch*" (insert string))
  ;(with-current-buffer "*scratch*" (insert "\n"))
  ;(with-current-buffer "*scratch*" (insert (format "table: %s" table)))
  ;; (with-current-buffer "*scratch*" (insert "\n"))
  ;; (with-current-buffer "*scratch*" (insert (format "prefix+pattern: %s"
  ;; 						   (orderless--prefix+pattern string table pred))))
  ;; (with-current-buffer "*scratch*" (insert "\n"))
  ;; (with-current-buffer "*scratch*" (insert (format "%s" (orderless-pattern-compiler string))))
  ;; (with-current-buffer "*scratch*" (insert "\n"))
  ;; (with-current-buffer "*scratch*" (insert "\n"))
  ;; (with-current-buffer "*scratch*" (insert (format "%s" (orderless-filter string table pred))))
  ;(orderless-all-completions string table 'grep-no-filename-pred _point)
  (orderless-all-completions string table 'grep-no-filename-pred _point)
  ;(with-current-buffer "*scratch*" (insert pred))
  ;(with-current-buffer "*scratch*" (insert point))
  )


;; (use-package selectrum
;;   :straight t
;;   :init
;;   (selectrum-mode +1))

(use-package vertico
  :straight t
  :init
  (vertico-mode +1)
  :config
  (add-to-list 'completion-ignored-extensions "#"))

;; (defun consult-company ()
;;   "Complete using `company-candidates'."
;;   (interactive)
;;   (company-mode 1)
;;   (unless company-candidates
;;     (company-complete))
;;   (let ((len (cond (company-common
;;                     (length company-common))
;;                    (company-prefix
;;                     (length company-prefix)))))
;;     (when len
;;       ;; (setq ivy-completion-beg (- (point) len))
;;       ;; (setq ivy-completion-end (point))
;;       (completing-read "Candidate: " company-candidates ))))

(defvar selectrum-search-rg-history nil)

(provide 'init-completion)
