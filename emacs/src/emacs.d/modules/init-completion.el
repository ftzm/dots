(use-package ag
  :straight t
  )

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
	     :branch "main"))
;  :straight (multi-libvterm
;	     :type git
;	     :host github
;	     :repo "suonlight/multi-libvterm"))

(use-package embark
  :straight t
  )

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

(defun consult-company ()
  "Complete using `company-candidates'."
  (interactive)
  (company-mode 1)
  (unless company-candidates
    (company-complete))
  (let ((len (cond (company-common
                    (length company-common))
                   (company-prefix
                    (length company-prefix)))))
    (when len
      ;; (setq ivy-completion-beg (- (point) len))
      ;; (setq ivy-completion-end (point))
      (completing-read "Candidate: " company-candidates ))))

(provide 'init-completion)