(use-package shackle
  :straight t
  :after (magit)
  :if (not (bound-and-true-p disable-pkg-shackle))
  :config
  (setq shackle-lighter "")
  (setq shackle-select-reused-windows nil) ; default nil
  (setq shackle-default-alignment 'below) ; default below
  (setq shackle-default-size 0.4) ; default 0.5

  (setq shackle-rules
        ;; CONDITION(:regexp)            :select     :inhibit-window-quit   :size+:align|:other     :same|:popup
        '(
      ;(compilation-mode              :select nil                                               )
          ;("*undo-tree*"                                                    :size 0.25 :align right)
          ;("*eshell*"                    :select t                          :other t               )
          ;("*Shell Command Output*"      :select nil                                               )
          ;("\\*Async Shell.*\\*" :regexp t :ignore t                                                 )
          ;(occur-mode                    :select nil                                   :align t    )
          ;("*Help*"                      :select t   :inhibit-window-quit t :other t               )
          ;("*Completions*"                                                  :size 0.3  :align t    )
          ;("*Messages*"                  :select nil :inhibit-window-quit t :other t               )
          ;("\\*[Wo]*Man.*\\*"    :regexp t :select t   :inhibit-window-quit t :other t               )
          ;("\\*poporg.*\\*"      :regexp t :select t                          :other t               )
          ;("*Calendar*"                  :select t                          :size 0.3  :align below)
          ;("*info*"                      :select t   :inhibit-window-quit t                         :same t)
          ;("*eldoc*"                  :select t                          :size 0.3  :align below)
          (magit-status-mode             :select t   :inhibit-window-quit t                         :same t)
          (magit-log-mode                :select t   :inhibit-window-quit t                         :same t)
          ))

  (shackle-mode 1)
  )

(use-package popper
  :straight t ; or :straight t
  :bind (("C-'"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*Eldoc\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints

(provide 'init-shackle)
