(use-package avy
  :straight t
  :config

  (defun avy-goto-char-flex (char1 char2 &optional arg beg end)
  "Behaves like avy-goto-char-2 unless the second character is RET,
in which case does avy-goto-char with the first char."
  (interactive (list (read-char "char 1: " t)
                     (read-char "char 2: " t)
                     current-prefix-arg
                     nil nil))
  (when (eq char1 ?)
    (setq char1 ?\n))
  (if (not (eq char2 ?))
      (avy-with avy-goto-char-2
      (avy-jump
       (regexp-quote (string char1 char2))
       :beg beg
       :end end))
      (avy-with avy-goto-char
      (avy-jump
       (regexp-quote (string char1))))))

  (setq avy-timeout-seconds 0.15)
  (setq avy-all-windows nil)
  (setq avy-keys '(?h ?u ?t ?e ?d ?i ?s ?a ?g ?p ?c ?. ?f ?y ?r ?, ?l ?' ?m
		      ?k ?w ?j ?b ?x ?v ?q ?z ?\; ?n ?o))

  (set-face-attribute 'avy-lead-face nil
  :background 'unspecified
  :foreground "#ff0000"
  )

  (set-face-attribute 'avy-lead-face-0 nil
  :background 'unspecified
  :foreground "#ff0000"
  )

  (setq avy-background t)
  (setq avy-all-windows t)

  )

(provide 'init-avy)
