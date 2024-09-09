;; Load and configure 'yasnippet' for snippet support
(use-package yasnippet
  :straight t
  :defer t
  :config
  (yas-global-mode))

;; Load and configure 'lsp-bridge' for Language Server Protocol support
(use-package lsp-bridge
  :straight (lsp-bridge :host github
			:repo "manateelazycat/lsp-bridge"
			:files ("*.el" "*.py" "acm" "core" "langserver"
				"multiserver" "resources"))
  :defer t)

;; Load and configure 'apheleia' for code formatting
(use-package apheleia
  :straight t
  :config
  (apheleia-global-mode))

;; For non-graphical environments, load 'popon' and 'acm-terminal'
(unless (display-graphic-p)
  (use-package popon
    :straight (popon :host nil :repo "https://codeberg.org/akib/emacs-popon.git")
    :defer t)
  (use-package acm-terminal
    :straight (acm-terminal :host github :repo "twlz0ne/acm-terminal")
    :after acm))
