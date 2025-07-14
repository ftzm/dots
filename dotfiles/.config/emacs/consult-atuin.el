;; -*- lexical-binding: t; -*-

(require 'consult)

(defface consult-atuin-datetime
  '((t :inherit font-lock-doc-face))
  "Face used to highlight the datetime in 'consult-atuin'."
  :group 'consult-atuin)

(defface consult-atuin-host
  '((t :inherit font-lock-type-face))
  "Face used to highlight the datetime in 'consult-atuin'."
  :group 'consult-atuin)

(defface consult-atuin-directory
  '((t :inherit completions-annotations))
  "Face used to highlight the datetime in 'consult-atuin'."
  :group 'consult-atuin)

(defface consult-atuin-duration
  '((t :inherit completions-annotations))
  "Face used to highlight the datetime in 'consult-atuin'."
  :group 'consult-atuin)

(defface consult-atuin-exit-error
  `((t
     :foreground ,(face-background 'default)
     :background ,(face-foreground 'error)))
  "Face used to highlight the datetime in 'consult-atuin'."
  :group 'consult-atuin)

(defface consult-atuin-exit-success
  '((t :inherit font-lock-string-face))
  "Face used to highlight the datetime in 'consult-atuin'."
  :group 'consult-atuin)

(defcustom consult-atuin-args
  "atuin search --search-mode full-text -r --format \"{time}@@@{host}@@@{directory}@@@{duration}@@@{exit}@@@{command}\"" ;; --existing not supported by Debian plocate
  "Command line arguments for locate, see `consult-locate'.
    The dynamically computed arguments are appended.
    Can be either a string, or a list of strings or expressions."
  :type '(choice string (repeat (choice string sexp))))

(defun atuin-builder (input)
  "Build command line from INPUT."
  (pcase-let ((`(,arg . ,opts) (consult--command-split input)))
    (unless (string-blank-p arg)
      (cons (append (consult--build-args consult-atuin-args)
                    (consult--split-escaped arg) opts)
            (cdr (consult--default-regexp-compiler input 'basic t))))))


(defun consult-atuin--format-line (str)
  (when (string-match "\\(.*\\)@@@\\(.*\\)@@@\\(.*\\)@@@\\(.*\\)@@@\\(.*\\)@@@\\(.*\\)" str)
    (let ((datetime (match-string 1 str))
          (host (match-string 2 str))
          (directory (match-string 3 str))
          (duration (match-string 4 str))
          (exit (match-string 5 str))
          (command (match-string 6 str)))
      (put-text-property 0
			 (length command)
			 'consult-atuin--metadata
			 `((datetime . ,datetime)
                           (host . ,host)
                           (directory . ,directory)
                           (duration . ,duration)
                           (exit . ,exit))
			 command)
      command
      )))

(defun consult-atuin-result-annotator (cand)
  "Annotate the current candidate CAND using its text-properties."
  (when-let (metadata (get-text-property 0 'consult-atuin--metadata cand))
    (let ((datetime (cdr (assoc 'datetime metadata)))
	  (exit (cdr (assoc 'exit metadata)))
          (host (cdr (assoc 'host metadata)))
          (directory (cdr (assoc 'directory metadata))))
      (format "%s %s  %s  %s"
	      (if (string= exit "0")
		  ""
		(format " %s " (propertize "x" 'face 'consult-atuin-exit-error))
		)
              (propertize datetime 'face 'consult-atuin-datetime)
              (propertize host 'face 'consult-atuin-host)
              (propertize directory 'face 'consult-atuin-directory)))))


(defun consult-atuin-format (lines)
  "Format git log grep candidates from LINES."
  (let ((candidates))
    (save-match-data
      (dolist (str lines)
					;(when (string-match "\\([a-z0-9].*\\)@@@\\(.*\\)@@@\\(.*\\)@@@\\(.*\\)" str)
        (when (string-match "\\(.*\\)@@@\\(.*\\)@@@\\(.*\\)@@@\\(.*\\)@@@\\(.*\\)@@@\\(.*\\)" str)
          (let ((datetime (match-string 1 str))
                (host (match-string 2 str))
                (directory (match-string 3 str))
                (duration (match-string 4 str))
                (exit (match-string 5 str))
                (command (match-string 6 str)))
            (put-text-property 0
			       1
			       'consult-atuin--metadata
			       `((datetime . ,datetime)
                                 (host . ,host)
                                 (directory . ,directory)
                                 (duration . ,duration)
                                 (exit . ,exit))
			       command)
            (push command candidates)))))
					;(nreverse candidates)
    candidates
    ))

(defun consult--atuin (prompt builder initial)
  "Run find command in current directory.

    The function returns the selected file.
    The filename at point is added to the future history.

    BUILDER is the command line builder function.
    PROMPT is the prompt.
    INITIAL is initial input."
  (consult--read
   (consult--process-collection builder
     ;;TODO verify that map is the right thing, as we can't reject non-compliant lines
     :transform (consult--async-map #'consult-atuin--format-line)
					;(consult--async-transform consult-atuin-format)
     (consult--async-highlight builder))
   :prompt prompt
   :sort nil
   :annotate 'consult-atuin-result-annotator 
					;:require-match t
   :initial initial
					;:add-history (consult--async-split-thingatpt 'filename)
					;:category 'file
					;:history '(:input consult--find-history)
   ))

;; https://jao.io/blog/consulting-spotify-in-a-better-way.html
;; for an overview of how the async consult--read function actually works
(defun consult-atuin (&optional initial)
  (interactive)
  (let ((consult-async-refresh-delay .0001)
	(consult-async-input-throttle .001)
	(consult-async-input-debounce .001)
	(consult-async-min-input 1))
    (goto-char (point-max))
    (insert (consult--atuin "Atuin: " #'atuin-builder "* "))))

