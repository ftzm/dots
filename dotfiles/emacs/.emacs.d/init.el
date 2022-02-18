(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))
(add-to-list 'load-path (format "%s/modules" user-emacs-directory))

(setq read-process-output-max (* 1024 1024))

(require 'init-modules)
;(setq gc-cons-threshold default-gc-threshold) ;Set back to normal

; workaround for native compilation bug--issue here: https://github.com/raxod502/straight.el/issues/680
; (setq comp-deferred-compilation-deny-list '("\\(?:[^z-a]*-autoloads\\.el$\\)"))

;Workarounds for evil mode--check if still necessary
;(setq max-specpdl-size 130000)
;(setq max-lisp-eval-depth 10000)

; Write custom set variables to separate file to keep things tidy.
; This is never actually loaded, but can be used as reference to implement
; things in intentional configuration.
(setq custom-file (concat user-emacs-directory "/custom.el"))

; don't show native comp warnings
(setq warning-suppress-types '((comp)))
