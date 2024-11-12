(use-package ag
  :straight t)

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(use-package consult
  :straight (consult
	     :branch "main")
  :config
  ;(setq completion-in-region-function #'consult-completion-in-region)
  ;; Will need to make sure this doesn't get out of date
  ;(setq consult-ripgrep-args "rg")

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


    ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (consult-customize
   ftzm/consult-ripgrep consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file ;consult-xref
   consult--source-bookmark
   :preview-key "M-.")
  )

(use-package embark
  :straight t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("M-." . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;(setq embark-prompter 'embark-completing-read-prompter)
(defun embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t (lambda (binding)
                   (not (string-suffix-p "-argument" (cdr binding))))))))

(setq embark-indicators
  '(embark-which-key-indicator
    embark-highlight-indicator
    embark-isearch-highlight-indicator))

(defun embark-hide-which-key-indicator (fn &rest args)
  "Hide the which-key indicator immediately when using the completing-read prompter."
  (which-key--hide-popup-ignore-command)
  (let ((embark-indicators
         (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

(advice-add #'embark-completing-read-prompter
            :around #'embark-hide-which-key-indicator)

(defvar-keymap embark-composer-actions
  :doc "Keymap for actions on composers."
  :parent embark-general-map
  "s" #'ftzm-mpd-composer-search
  "r" #'ftzm-mpd-composer-findadd)

(assq-delete-all 'composer embark-keymap-alist)
(add-to-list 'embark-keymap-alist '(composer . embark-composer-actions))


  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :straight t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

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
  :custom (completion-styles '(orderless))
  :config
  (setq orderless-smart-case t)
  (setq completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  )

(use-package vertico
  :ensure t ; use straight to install extensions is not registerd in elpa
  :straight (:type git :repo "minad/vertico"
	     :files (:defaults "extensions/*")
                        :includes (vertico-buffer
                                   vertico-directory
                                   vertico-multiform
                                   vertico-flat
                                   vertico-indexed
                                   vertico-mouse
                                   vertico-quick
                                   vertico-repeat
                                   vertico-reverse))
  :init
  (vertico-mode)
  :config
;; Use `consult-completion-in-region' if Vertico is enabled.
;; Otherwise use the default `completion--in-region' function.
(setq completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
               args)))

  (require 'vertico-quick)

  (require 'vertico-directory)
  (define-key vertico-map "\C-q" #'vertico-quick-exit)
  (define-key vertico-map "\d" #'vertico-directory-delete-char)
  (define-key vertico-map "\M-\d" #'vertico-directory-delete-word)

  (add-to-list 'completion-ignored-extensions "#")

  ;; don't sort the candidates in these functions
  (require 'vertico-multiform)
  (setq vertico-multiform-commands '((ftzm/bash-history (vertico-sort-function . nil))))
  (vertico-multiform-mode)


  )

;; Configure directory extension.
;(use-package vertico-directory
;  :after vertico
;  :ensure nil
;  ;; More convenient directory navigation commands
;  :bind (:map vertico-map
;              ("RET" . vertico-directory-enter)
;              ("DEL" . vertico-directory-delete-char)
;              ("M-DEL" . vertico-directory-delete-word))
;  ;; Tidy shadowed file names
;  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; (use-package vertico-multiform
;;   :after vertico
;;   :ensure nil
;;   :config
;;   ;; don't sort the candidates in these functions
;;   (setq vertico-multiform-commands '((ftzm/bash-history (vertico-sort-function . nil))))
;;   (vertico-multiform-mode)
;;   )


(use-package consult-dir
  :straight t
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))
  :config
  (setq consult-dir-shadow-filenames nil) ;don't leave "shadowed" original
					;search text
  )

(defun try-custom ()
  (delete-minibuffer-contents)
  (insert "/var/"))

(provide 'init-completion)
