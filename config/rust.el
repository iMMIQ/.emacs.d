(add-to-list 'projectile-project-root-files "Cargo.toml")

(use-package rustic
  :straight t
  :defer t
  :mode ("\\.rs$" . rustic-mode)
  :preface
  (setq rustic-lsp-client nil)
  (remove-hook 'rustic-mode-hook 'rustic-setup-lsp)
  (remove-hook 'rustic-mode-hook #'flycheck-mode)
  (remove-hook 'rustic-mode-hook #'flymake-mode-off)
  :hook
  (rustic-mode . lsp-bridge-mode)
  :config
  (when (package-installed-p 'rainbow-delimiters)
    (add-hook 'rustc-mode-hook #'rainbow-delimiters-mode))
  (setq rustic-indent-method-chain t)
  (setq rustic-babel-format-src-block nil
	rustic-format-trigger nil)
  (setq lsp-bridge-enable-inlay-hint t))
