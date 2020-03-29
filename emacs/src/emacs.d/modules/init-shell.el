(use-package shell
  :defer t
  :commands shell
  :config
  (setq explicit-shell-file-name (getenv "SHELL"))
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
  (add-hook 'shell-mode-hook
      (lambda ()
        (face-remap-set-base 'comint-highlight-prompt :inherit nil)))

  (add-to-list 'display-buffer-alist
               `(,(regexp-quote "*shell") display-buffer-same-window))

  (setq comint-prompt-read-only nil
        ;;; Remember lots of previous commands in shell-mode
        comint-input-ring-size 1000000
        comint-input-ignoredups t)

  (defun my-shell-mode-hook ()
    "A hook to setup shell-mode."
    ;;(setq comint-output-filter-functions
    ;;  (remove'ansi-color-process-output comint-output-filter-functions))
    ;;;;(add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)
    ;;; Removes face inherited from minibuffer
    (face-remap-set-base 'comint-highlight-prompt :inherit nil)
    ;;; You can highlight some text based on regexp (useful to see "OK" or warnings):
    ;; (add-hook 'shell-mode-hook (lambda () (highlight-regexp "\\[OK\\]" "hi-green-b")))
    ;;; Make URLs clickable
    (goto-address-mode)
    ;;; Make file paths clickable
    ;;; Every line representing a path to a file will be colorized and made clickable, so that you can jump to that file and that line, like in compilation-mode (specially useful when compiling a program or running tests):
    (compilation-shell-minor-mode)

    ;;; For some reasons must be in a hook
    (setq comint-input-ring-file-name "~/.zsh_history")
                                      ; Ignore timestamps in history file.  Assumes that zsh
                                      ; EXTENDED_HISTORY option is in use.
    (setq comint-input-ring-separator "\n: \\([0-9]+\\):\\([0-9]+\\);")

    (comint-read-input-ring t)
    )
  (add-hook 'shell-mode-hook 'my-shell-mode-hook)
  (define-key shell-mode-map (kbd "C-r") 'counsel-shell-history)
  )

(use-package ansi-color
  :after shell
  )

(provide 'init-shell)
