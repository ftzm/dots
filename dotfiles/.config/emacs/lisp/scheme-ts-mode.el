;;; scheme-ts-mode.el --- Tree-sitter support for Scheme -*- lexical-binding: t; -*-

;;; Commentary:
;; A tree-sitter based major mode for Scheme, derived from prog-mode.

;;; Code:

(require 'treesit)
(require 'scheme)

(defvar scheme-ts-mode--keywords
  '("define" "define-syntax" "lambda" "λ"
    "let" "let*" "letrec" "letrec-syntax" "let-syntax"
    "let-values" "let*-values"
    "if" "cond" "case" "else" "=>" "when" "unless"
    "and" "or" "not"
    "begin" "do"
    "set!"
    "quote" "unquote" "quasiquote" "quote-splicing" "unquote-splicing"
    "syntax-rules" "identifier-syntax"
    "define-record-type" "define-values"
    "import" "export" "library" "rename" "only" "except" "prefix"
    "delay" "delay-force" "force"
    "assert"
    "guard" "dynamic-wind"
    "parameterize" "make-parameter"
    "call-with-current-continuation" "call/cc"
    "call-with-values" "values"
    "with-exception-handler" "raise" "raise-continuable"
    "include" "include-ci"
    "cond-expand" "features"
    "define-library" "begin"))

(defvar scheme-ts-mode--keyword-regexp
  (concat "^" (regexp-opt scheme-ts-mode--keywords) "$")
  "Regexp matching Scheme keywords.")

(defvar scheme-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'scheme
   :feature 'comment
   '((comment) @font-lock-comment-face
     (block_comment) @font-lock-comment-face
     (directive) @font-lock-preprocessor-face)

   :language 'scheme
   :feature 'string
   '((string) @font-lock-string-face
     (escape_sequence) @font-lock-escape-face)

   :language 'scheme
   :feature 'number
   '((number) @font-lock-number-face
     (character) @font-lock-constant-face
     (boolean) @font-lock-constant-face)

   :language 'scheme
   :feature 'keyword
   :override t
   (format "(list . (symbol) @font-lock-keyword-face (#match %S @font-lock-keyword-face))"
           scheme-ts-mode--keyword-regexp)

   :language 'scheme
   :feature 'definition
   "(list . (symbol) @_kw . (symbol) @font-lock-function-name-face (#match \"^define\" @_kw))
    (list . (symbol) @_kw . (list . (symbol) @font-lock-function-name-face) (#match \"^define\" @_kw))"

   :language 'scheme
   :feature 'function
   "(list . (symbol) @font-lock-function-call-face)"

   :language 'scheme
   :feature 'quote
   '((quote _ @font-lock-constant-face)
     (quote (_ _ @font-lock-constant-face))
     (quote (_ (_ _ @font-lock-constant-face))))

   :language 'scheme
   :feature 'bracket
   '(["(" ")" "[" "]" "{" "}"] @font-lock-bracket-face)

   :language 'scheme
   :feature 'operator
   '(((symbol) @font-lock-operator-face
      (:match "^[+\\-*/=<>]=?$" @font-lock-operator-face))))
  "Font-lock settings for `scheme-ts-mode'.")

(defvar scheme-ts-mode--indent-rules
  `((scheme
     ((parent-is "program") column-0 0)
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((parent-is "list") parent-bol 1)
     ((parent-is "quote") parent-bol 1)
     (no-node parent-bol 1)))
  "Indentation rules for `scheme-ts-mode'.")

;;;###autoload
(define-derived-mode scheme-ts-mode prog-mode "Scheme"
  "Major mode for editing Scheme, powered by tree-sitter."
  :syntax-table scheme-mode-syntax-table

  (when (treesit-ready-p 'scheme)
    (treesit-parser-create 'scheme)

    (setq-local treesit-font-lock-settings scheme-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '((comment string)
                  (keyword definition number)
                  (function quote)
                  (bracket operator)))
    (setq-local treesit-simple-indent-rules scheme-ts-mode--indent-rules)

    (setq-local comment-start ";; ")
    (setq-local comment-end "")

    (setq-local treesit-simple-imenu-settings
                '(("Function" "\\`list\\'" nil
                   (lambda (node)
                     (let ((first-child (treesit-node-child node 1)))
                       (and first-child
                            (equal (treesit-node-type first-child) "symbol")
                            (string-match "^define" (treesit-node-text first-child))))))))

    (treesit-major-mode-setup)))

(if (treesit-ready-p 'scheme t)
    (progn
      (add-to-list 'auto-mode-alist '("\\.scm\\'" . scheme-ts-mode))
      (add-to-list 'auto-mode-alist '("\\.sld\\'" . scheme-ts-mode))))

(provide 'scheme-ts-mode)
;;; scheme-ts-mode.el ends here
