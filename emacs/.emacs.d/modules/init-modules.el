;;global
(require 'init-gui)
(require 'init-straight)
(require 'init-gui-pkg)
(require 'init-avy)
(require 'init-themes)
(require 'init-evil)
(require 'init-shackle)
(require 'init-ivy)
(require 'init-utils)
(require 'init-persp)
(require 'init-magit)
(require 'init-settings)
(require 'init-projectile)
(require 'init-neotree) ;disabled
(require 'init-spaceline)
(require 'init-flycheck)
(require 'init-company)

;;context specific
(require 'init-org)
(require 'init-elisp)
(require 'init-nix)
(require 'init-json)
(require 'init-haskell)

;;centralized key mappings
(require 'init-keys)

(provide 'init-modules)
