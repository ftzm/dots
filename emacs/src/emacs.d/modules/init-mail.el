(use-package mu4e
  :load-path "/run/current-system/sw/share/emacs/site-lisp/mu4e/"
  :config
  ;;location of my maildir

  (setq mu4e-get-mail-command "mbsync -a")

  (setq mu4e-html2text-command "w3m -dump -s -T text/html -o display_link_number=true")

  (add-to-list 'mu4e-view-actions
  '("ViewInBrowser" . mu4e-action-view-in-browser) t)

  (setq mu4e-maildir "~/.mbsync"
      mu4e-contexts
    `( ,(make-mu4e-context
         :name "fitzmattd"
         :match-func (lambda (msg) (when msg (mu4e-message-contact-field-matches msg :to "fitz.matt.d@gmail.com")))
         :vars '((mu4e-trash-folder      . "/gmail-fitzmattd/trash")
		 (mu4e-sent-folder      . "/gmail-fitzmattd/sent")
		 (mu4e-sent-folder      . "/gmail-fitzmattd/drafts")

		 ; sending mail
		 (message-send-mail-function . smtpmail-send-it)
		 (smtpmail-stream-type . starttls)
		 (smtpmail-default-smtp-server . "smtp.gmail.com")
		 (smtpmail-smtp-server . "smtp.gmail.com")
		 (smtpmail-smtp-service . 587)
		 (smtpmail-smtp-user . "fitz.matt.d@gmail.com")

		 ; (mu4e-bookmarks . (("maildir:/gmail-fitzmattd/inbox" "Inbox" ?i)
		 ; 		   ))
		 )
	 )
       ,(make-mu4e-context
         :name "tempo"
         :match-func (lambda (msg) (when msg (mu4e-message-contact-field-matches msg :to "mfitzsimmons@tempo.io")))
         :vars '(
		 ; sending mail
		 (message-send-mail-function . smtpmail-send-it)
		 (smtpmail-stream-type . starttls)
		 (smtpmail-default-smtp-server . "smtp.gmail.com")
		 (smtpmail-smtp-server . "smtp.gmail.com")
		 (smtpmail-smtp-service . 587)
		 (smtpmail-smtp-user . "mfitzsimmons@tempo.io")

		 ; (mu4e-bookmarks . (("maildir:/gmail-tempo/inbox" "Inbox" ?i)
		 ; 		   ))

                 (mu4e-maildir-shortcuts . (("/gmail-tempo/inbox"     . ?i)
                                            ("/gmail-tempo/archive"   . ?a)
                                            ("/gmail-tempo/trash"   . ?a)
                                            ("/gmail-tempo/sent"      . ?s)))
		 )
	 )
       )
    )
  )

(provide 'init-mail)
