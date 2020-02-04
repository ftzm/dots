(use-package mu4e
  :load-path "/run/current-system/sw/share/emacs/site-lisp/mu4e/"
  :config

  (setq mu4e-change-filenames-when-moving t)
  (setq mu4e-compose-format-flowed t) ;no automatic newlines

  (setq message-citation-line-format "> On %d %b %Y at %R, %f wrote:\n>")
  (setq message-citation-line-function 'message-insert-formatted-citation-line)

  (require 'org-mu4e)
  (define-key mu4e-headers-mode-map (kbd "C-c c") 'org-mu4e-store-and-capture)
  (define-key mu4e-view-mode-map    (kbd "C-c c") 'org-mu4e-store-and-capture)

  (setq mu4e-get-mail-command "mbsync -a")

  (setq mu4e-html2text-command "w3m -dump -s -T text/html -o display_link_number=true")

  (add-to-list 'mu4e-view-actions
  '("ViewInBrowser" . mu4e-action-view-in-browser) t)

  ;; start with the first (default) context;
  ;; default is to ask-if-none (ask when there's no context yet, and none match)
  (setq mu4e-context-policy 'pick-first)

  ;; compose with the current context is no context matches;
  ;; default is to ask
  (setq mu4e-compose-context-policy nil)

  (setq
   mu4e-maildir "~/.maildir"
   mu4e-contexts
   `(
     ,(make-mu4e-context
        :name "org"
	:match-func (lambda (msg) (when msg (mu4e-message-contact-field-matches msg
									   :to
									   "m@ftzm.org")))
	 :vars '((mu4e-trash-folder . "/ftzm/Trash")
		 (mu4e-sent-folder . "/ftzm/Sent")
		 (mu4e-refile-folder . "/ftzm/Archive")

		 (mu4e-change-filenames-when-moving . t)

		 (user-mail-address . "m@ftzm.org")
		 (user-full-name . "Matthew Fitzsimmons")
		 (sendmail-program . "msmtp")
		 (message-sendmail-f-is-evil . t)
		 (message-sendmail-extra-arguments . ("--read-envelope-from"))
		 (message-send-mail-function . message-send-mail-with-sendmail)
		 (mu4e-sent-messages-behavior . sent)

		 (mu4e-bookmarks
		  . (("maildir:/ftzm/Inbox" "Inbox" ?i)
		     ("maildir:/ftzm/Archive" "Archive" ?a)))
		 )
	 )
      ,(make-mu4e-context
         :name "fitzmattd"
         :match-func (lambda (msg) (when msg (mu4e-message-contact-field-matches msg :to "fitz.matt.d@gmail.com")))
         :vars '((mu4e-trash-folder      . "/fitzmattd/[Gmail]/Trash")
		 (mu4e-sent-folder      . "/fitzmattd/[Gmail]/Sent Mail")
		 (mu4e-drafts-folder      . "/fitzmattd/[Gmail]/Drafts")
		 (mu4e-refile-folder      . "/fitzmattd/Annals")

		 (mu4e-change-filenames-when-moving . t)

		 ; sending mail
		 (message-send-mail-function . smtpmail-send-it)
		 (smtpmail-stream-type . starttls)
		 (smtpmail-default-smtp-server . "smtp.gmail.com")
		 (smtpmail-smtp-server . "smtp.gmail.com")
		 (smtpmail-smtp-service . 587)
		 (smtpmail-smtp-user . "fitz.matt.d@gmail.com")
		 (mu4e-sent-messages-behavior . delete)

		 (mu4e-bookmarks . (("maildir:/fitzmattd/Inbox" "Inbox" ?i)))
		 )
	 )
      )
    )
  )

(provide 'init-mail)
