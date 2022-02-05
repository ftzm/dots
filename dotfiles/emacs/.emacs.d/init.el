(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

;(setq default-gc-threshold gc-cons-threshold)
;(setq gc-cons-threshold 200000000) ;High GC for fast startup
(setq read-process-output-max (* 1024 1024))
(add-to-list 'load-path (format "%s/modules" user-emacs-directory))
(require 'init-modules)
;(setq gc-cons-threshold default-gc-threshold) ;Set back to normal

; workaround for native compilation bug--issue here: https://github.com/raxod502/straight.el/issues/680
(setq comp-deferred-compilation-deny-list '("\\(?:[^z-a]*-autoloads\\.el$\\)"))

;Workarounds for evil mode--check if still necessary
(setq max-specpdl-size 130000)
(setq max-lisp-eval-depth 10000)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(awesome-tray-mode-line-active-color "#2fafff")
 '(awesome-tray-mode-line-inactive-color "#323232")
 '(blink-cursor-mode nil)
 '(custom-safe-themes
   '("6b5c518d1c250a8ce17463b7e435e9e20faa84f3f7defba8b579d4f5925f60c1" "83e0376b5df8d6a3fbdfffb9fb0e8cf41a11799d9471293a810deb7586c131e6" "c44380b9483c0808694bc2a721ca626cc1925f51f580dcefbc0fc072f1906111" "a21dcd2f5b8c38e799b9dcb91f702d5d864263a794732aa3ac05c73de070d1da" "7661b762556018a44a29477b84757994d8386d6edee909409fabe0631952dad9" "d91ef4e714f05fff2070da7ca452980999f5361209e679ee988e3c432df24347" "585942bb24cab2d4b2f74977ac3ba6ddbd888e3776b9d2f993c5704aa8bb4739" "8f97d5ec8a774485296e366fdde6ff5589cf9e319a584b845b6f7fa788c9fa9a" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "8e797edd9fa9afec181efbfeeebf96aeafbd11b69c4c85fa229bb5b9f7f7e66c" "b583823b9ee1573074e7cbfd63623fe844030d911e9279a7c8a5d16de7df0ed0" "2b9dc43b786e36f68a9fd4b36dd050509a0e32fe3b0a803310661edb7402b8b6" "4e78818bf9e231d3f0d03798886298837137bbbde0f3daf343ec2ec85dc90d91" "4feee83c4fbbe8b827650d0f9af4ba7da903a5d117d849a3ccee88262805f40d" "3da031b25828b115c6b50bb92a117f5c0bbd3d9d0e9ba5af3cd2cb9db80db1c2" "fd3c7bd752f48dcb7efa5f852ef858c425b1c397b73851ff8816c0580eab92f1" "8e0c6a96a17a5b45979c31265821053aff9beea9fb5ac5e41130e0c27a89214e" "d64b20a5b3c0abc22a5f0945a4e4aa7dd25f971e587a760316a73ca851d7e82f" "cd4d1a0656fee24dc062b997f54d6f9b7da8f6dc8053ac858f15820f9a04a679" "7d2e7a9a7944fbde74be3e133fc607f59fdbbab798d13bd7a05e38d35ce0db8d" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "d1b4990bd599f5e2186c3f75769a2c5334063e9e541e37514942c27975700370" "1c082c9b84449e54af757bcae23617d11f563fc9f33a832a8a2813c4d7dfb652" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" "ecba61c2239fbef776a72b65295b88e5534e458dfe3e6d7d9f9cb353448a569e" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "4697a2d4afca3f5ed4fdf5f715e36a6cac5c6154e105f3596b44a4874ae52c45" "75d3dde259ce79660bac8e9e237b55674b910b470f313cdf4b019230d01a982a" "356e5cbe0874b444263f3e1f9fffd4ae4c82c1b07fe085ba26e2a6d332db34dd" "93a0885d5f46d2aeac12bf6be1754faa7d5e28b27926b8aa812840fe7d0b7983" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "fe666e5ac37c2dfcf80074e88b9252c71a22b6f5d2f566df9a7aa4f9bea55ef8" "b35a14c7d94c1f411890d45edfb9dc1bd61c5becd5c326790b51df6ebf60f402" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "7366916327c60fdf17b53b4ac7f565866c38e1b4a27345fe7facbf16b7a4e9e8" "3fa81193ab414a4d54cde427c2662337c2cab5dd4eb17ffff0d90bca97581eb6" "6ac7c0f959f0d7853915012e78ff70150bfbe2a69a1b703c3ac4184f9ae3ae02" "0c58b8e826a6ae7f3b7f5cc08a4ad750d2df4441fb85e08d3f387b0823fd6960" "42b9d85321f5a152a6aef0cc8173e701f572175d6711361955ecfb4943fe93af" "021720af46e6e78e2be7875b2b5b05344f4e21fad70d17af7acfd6922386b61e" default))
 '(epg-gpg-program (executable-find "gpg"))
 '(exwm-floating-border-color "#646464")
 '(flymake-error-bitmap '(flymake-double-exclamation-mark modus-themes-fringe-red))
 '(flymake-note-bitmap '(exclamation-mark modus-themes-fringe-cyan))
 '(flymake-warning-bitmap '(exclamation-mark modus-themes-fringe-yellow))
 '(highlight-tail-colors '(("#2f4a00" . 0) ("#00415e" . 20)))
 '(hl-todo-keyword-faces
   '(("HOLD" . "#c0c530")
     ("TODO" . "#feacd0")
     ("NEXT" . "#b6a0ff")
     ("THEM" . "#f78fe7")
     ("PROG" . "#00d3d0")
     ("OKAY" . "#4ae2f0")
     ("DONT" . "#70b900")
     ("FAIL" . "#ff8059")
     ("BUG" . "#ff8059")
     ("DONE" . "#44bc44")
     ("NOTE" . "#d3b55f")
     ("KLUDGE" . "#d0bc00")
     ("HACK" . "#d0bc00")
     ("TEMP" . "#ffcccc")
     ("FIXME" . "#ff9077")
     ("XXX+" . "#ef8b50")
     ("REVIEW" . "#6ae4b9")
     ("DEPRECATED" . "#bfd9ff")))
 '(ibuffer-deletion-face 'modus-themes-mark-del)
 '(ibuffer-filter-group-name-face 'modus-themes-pseudo-header)
 '(ibuffer-marked-face 'modus-themes-mark-sel)
 '(ibuffer-title-face 'default)
 '(org-src-block-faces 'nil)
 '(pdf-view-midnight-colors '("#ffffff" . "#100f10"))
 '(safe-local-variable-values
   '((dante-methods bare-cabal)
     (dante-methods new-flake)
     (dante-methods impure-nix)
     (dante-repl-command-line "nix-shell"
			      (concat
			       (expand-file-name
				(vc-root-dir))
			       "shell.nix")
			      "--run" "cabal new-repl --builddir=dist/dante")
     (dante-repl-command-line "nix-shell" "--attr" "pipestatus.env"
			      (concat
			       (expand-file-name
				(vc-root-dir))
			       "shell.nix")
			      "--run" "cabal new-repl --builddir=dist/dante")
     (dante-methods nix-ghci)
     (slime-lisp-implementations
      (sbcl
       ("sbcl" "--userinit" ".sbclrc")))
     (slime-lisp-implementations
      (sbcl
       ("sbcl")
       "--userinit" ".sbclrc"))
     (slime-lisp-implementations
      (sbcl
       ("sbcl")))
     (slime-lisp-implementations
      (sbcl
       ((executable-find "sbcl"))))
     (slime-lisp-implementations
      (sbcl
       (executable-find "sbcl")))
     (slime-lisp-implementations quote
				 (sbcl
				  (executable-find "sbcl")))
     (slime-lisp-implementations sbcl
				 (executable-find "sbcl"))
     (inferior-lisp-program\.
      ("sbcl --userinit '.sbclrc'"))
     (eval progn
	   (pp-buffer)
	   (indent-buffer))))
 '(send-mail-function 'smtpmail-send-it)
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#ff8059")
     (40 . "#feacd0")
     (60 . "#f78fe7")
     (80 . "#ef8b50")
     (100 . "#d0bc00")
     (120 . "#c0c530")
     (140 . "#f8dec0")
     (160 . "#bfebe0")
     (180 . "#44bc44")
     (200 . "#70b900")
     (220 . "#6ae4b9")
     (240 . "#4ae2f0")
     (260 . "#00d3d0")
     (280 . "#c6eaff")
     (300 . "#2fafff")
     (320 . "#79a8ff")
     (340 . "#00bcff")
     (360 . "#b6a0ff")))
 '(vc-annotate-very-old-color nil)
 '(xterm-color-names
   ["black" "#ff8059" "#44bc44" "#d0bc00" "#2fafff" "#feacd0" "#00d3d0" "gray65"])
 '(xterm-color-names-bright
   ["gray35" "#ef8b50" "#70b900" "#c0c530" "#79a8ff" "#f78fe7" "#4ae2f0" "white"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block-begin-line ((t (:inherit font-lock-comment-face :background nil))))
 '(org-block-end-line ((t (:inherit font-lock-comment-face :background nil))))
 '(org-level-2 ((t (:foreground nil :inherit org-level-1))))
 '(org-level-3 ((t (:foreground nil :inherit org-level-1)))))
