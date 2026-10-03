;; -*- lexical-binding: t; -*-

;;;;; Startup optimizations

;;;;;; Set garbage collection threshold

;; From https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/

(setq gc-cons-threshold (* 1024 1024 1000))

;;;;;; Set file-name-handler-alist

;; Also from https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/

(setq file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;;;;;; Set deferred timer to reset them

(run-with-idle-timer
 5 nil
 (lambda ()
   ;; 100MB permanent - modern recommendation. High enough to avoid frequent GC pauses,
   ;; low enough that individual collections don't cause noticeable freezes.
   (setq gc-cons-threshold (* 100 1024 1024))
   (setq file-name-handler-alist file-name-handler-alist-original)
   (makunbound 'file-name-handler-alist-original)
   (message "gc-cons-threshold and file-name-handler-alist restored")))

;;;;;; done

;; We don't want package.el when we use elpaca
(setq package-enable-at-startup nil)

;; Disable some GUI distractions. We set these manually to avoid starting
;; the corresponding minor modes.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . nil) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)

;; ui
(setq inhibit-startup-screen t)
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)


;;;;;; Rebuild elpaca packages after an Emacs version upgrade

;; Byte-compiled .elc files are not portable across Emacs versions, and nothing
;; invalidates them on an upgrade: elpaca keys its build cache on each package's
;; source revision, and Emacs's loader only compares .el/.elc mtimes -- the
;; ";;; in Emacs version 30.2" header is a comment, not a check.  Native
;; compilation, by contrast, *is* version-keyed (eln-cache/<version>-<hash>/), so
;; after an upgrade the two halves disagree: natively-compiled functions from the
;; new Emacs get late-loaded over top-level definitions that came from the old
;; Emacs's .elc.  That is how 30.2 -> 31.1 produced "Symbol's value as variable is
;; void: treesit-auto-mode--set-explicitly" on every major-mode change (Emacs 31
;; renamed the `define-globalized-minor-mode' internal MODE-set-explicitly to
;; MODE--set-explicitly).
;;
;; So stamp the Emacs version beside the build tree and, when it changes, drop the
;; build tree plus any foreign-version eln caches.  Elpaca rebuilds everything
;; from elpaca/sources/ during this same startup; nothing is re-cloned.

(let* ((elpaca-dir (expand-file-name "elpaca/" user-emacs-directory))
       (builds (expand-file-name "builds/" elpaca-dir))
       (stamp (expand-file-name "emacs-version" elpaca-dir))
       (eln-cache (expand-file-name "eln-cache/" user-emacs-directory))
       (recorded (and (file-readable-p stamp)
                      (with-temp-buffer
                        (insert-file-contents stamp)
                        (car (split-string (buffer-string) "\n" t))))))
  (unless (equal recorded emacs-version)
    ;; Only wipe when we know a *different* Emacs built the tree.  A missing
    ;; stamp means this guard is new (or the checkout is fresh), not stale.
    (when (and recorded (file-directory-p builds))
      (message "Emacs %s but elpaca builds are from %s; wiping %s to force a rebuild"
               emacs-version recorded builds)
      (delete-directory builds t))
    (when (and recorded (file-directory-p eln-cache))
      (dolist (dir (directory-files eln-cache t "\\`[0-9]"))
        (when (and (file-directory-p dir)
                   (not (string-prefix-p (concat emacs-version "-")
                                         (file-name-nondirectory dir))))
          (delete-directory dir t))))
    (make-directory elpaca-dir t)
    (with-temp-file stamp (insert emacs-version "\n"))))
