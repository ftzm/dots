;; linting appears a bit of a shit show, luacheck seems standard but spits
;; endless shit about ngx being an undefined variable. Maybe fix down the line.

(use-package lua-mode
  :straight t
  :config
  (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
  )

(use-package company-lua
  :straight t
  :config
  (add-to-list 'company-backends 'company-lua)
  )

(provide 'init-lua)
