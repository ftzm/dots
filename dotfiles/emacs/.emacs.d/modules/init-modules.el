;;global
(require 'init-gui)
(require 'init-straight)
(require 'init-themes)
(require 'init-powerline)
(require 'init-gui-pkg)
(require 'init-evil)
(require 'init-hydra)
(require 'init-avy)
(require 'init-shackle)
(require 'init-completion)
(require 'init-utils)
(require 'init-magit)
(require 'init-persp)
(require 'init-settings)
;(require 'init-projectile)
(require 'init-flycheck)
(require 'init-snippets)
(require 'init-company)
(require 'init-security)
(require 'init-spell)
(require 'init-direnv)

; tangled literate config
(require 'literate-config)

; Languages
(require 'init-org) ; Loading org later/below this point breaks it. There must
        	    ; be a loading order incompatibility with one of the
        	    ; modules below.
(require 'init-elisp)
(require 'init-lisp)
(require 'init-nix)
(require 'init-haskell)
(require 'init-purescript)
(require 'init-scala)
(require 'init-lsp)
(require 'init-elm)
(require 'init-python)
(require 'init-go)
(require 'init-lua)
(require 'init-racket)
(require 'init-js)
(require 'init-yaml)
(require 'init-json)
(require 'init-dhall)
(require 'init-jsonnet)
(require 'init-markdown)
(require 'init-prose)
(require 'init-dockerfile)
(require 'init-terraform)
(require 'init-copilot)
(require 'init-rust)

; Applications
(require 'init-shell)
(require 'init-mail)
(require 'init-dired)
;(require 'init-pdf)
;(require 'init-jira)
(require 'init-mpd)
;(require 'init-slack)
;(require 'init-telega)
;(require 'init-music)
(require 'init-vterm)


;;;centralized key mappings
(require 'init-keys)


(provide 'init-modules)
