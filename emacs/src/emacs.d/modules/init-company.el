(use-package company
  :straight t
  :diminish company-mode
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay nil)
  (define-key evil-insert-state-map "\C-n" 'company-complete)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (setq company-minimum-prefix-length 1)
  (setq company-require-match 'never) ;; allow breaking out by typing
  ;; Don't convert everything to lower case
  (setq company-dabbrev-downcase nil)

  (setq lsp-completion-provider :capf)


;
;
;  (company-tng-mode)
;
;  ;; set default `company-backends'
;  (setq company-backends
;	'((company-files          ; files & directory
;	   company-keywords       ; keywords
;	   company-capf
;	   ;;company-yasnippet ;;not using atm
;	   )
;	  (company-abbrev company-dabbrev)
;	  ))
;
;  (add-hook 'python-mode-hook
;            (lambda ()
;              (add-to-list (make-local-variable 'company-backends)
;                           'company-anaconda)))
;  (add-hook 'elisp-mode-hook
;            (lambda ()
;              (add-to-list (make-local-variable 'company-backends)
;                           'company-elisp)))
;
;  (add-hook 'elm-mode-hook
;            (lambda ()
;              (add-to-list (make-local-variable 'company-backends)
;                           'company-elm)))

  ;; Below: more involved specification of backends that I may employ later

  ;;(dolist (hook '(js-mode-hook
  ;;                js2-mode-hook
  ;;                js3-mode-hook
  ;;                inferior-js-mode-hook
  ;;                ))
  ;;  (add-hook hook
  ;;            (lambda ()
  ;;              (tern-mode t)
  ;;
  ;;              (add-to-list (make-local-variable 'company-backends)
  ;;                           'company-tern)
  ;;              )))

;;;;;_. company-mode support like auto-complete in web-mode
  ;;
;;;; Enable CSS completion between <style>...</style>
  ;;(defadvice company-css (before web-mode-set-up-ac-sources activate)
  ;;  "Set CSS completion based on current language before running `company-css'."
  ;;  (if (equal major-mode 'web-mode)
  ;;      (let ((web-mode-cur-language (web-mode-language-at-pos)))
  ;;        (if (string= web-mode-cur-language "css")
  ;;            (unless css-mode (css-mode))))))
  ;;
;;;; Enable JavaScript completion between <script>...</script> etc.
  ;;(defadvice company-tern (before web-mode-set-up-ac-sources activate)
  ;;  "Set `tern-mode' based on current language before running `company-tern'."
  ;;  (if (equal major-mode 'web-mode)
  ;;      (let ((web-mode-cur-language (web-mode-language-at-pos)))
  ;;        (if (or (string= web-mode-cur-language "javascript")
  ;;               (string= web-mode-cur-language "jsx"))
  ;;            (unless tern-mode (tern-mode))
  ;;          ;; (if tern-mode (tern-mode))
  ;;          ))))


  )

(use-package company-box
  :straight t
  :hook (company-mode . company-box-mode))

(provide 'init-company)
