;; Load and configure 'yasnippet' for snippet support
(use-package yasnippet
  :straight t
  :defer 3
  :diminish yas-minor-mode
  :config
  (yas-global-mode))

;; Load and configure 'lsp-bridge' for Language Server Protocol support
(use-package lsp-bridge
  :straight (lsp-bridge :host github
			:repo "manateelazycat/lsp-bridge"
			:files ("*.el" "*.py" "acm" "core" "langserver"
				"multiserver" "resources"))
  :defer t
  :init
  (setq lsp-bridge-enable-log nil
        lsp-bridge-enable-hover-diagnostic t
        lsp-bridge-enable-signature-help t
        lsp-bridge-completion-stop-when-inserting t)
  :config
  ;; ====================
  ;; LSP Keybindings (LazyVim-style)
  ;; ====================

  ;; Code actions (SPC c)
  (emacs-leader
    :states 'normal
    :keymaps 'override
    "ca" '(lsp-bridge-code-action :which-key "code action")
    "cr" '(lsp-bridge-rename :which-key "rename")
    "cD" '(lsp-bridge-diagnostic-list :which-key "diagnostic list")
    "cf" '(apheleia-format :which-key "format"))

  ;; Go to (g prefix - overrides evil)
  (general-def
    :states 'normal
    :keymaps 'override
    "gd" '(lsp-bridge-find-def :which-key "go to definition")
    "gD" '(lsp-bridge-find-def-other-window :which-key "definition (other window)")
    "gt" '(lsp-bridge-find-type-def :which-key "go to type definition")
    "gT" '(lsp-bridge-find-type-def-other-window :which-key "type definition (other window)")
    "gi" '(lsp-bridge-find-impl :which-key "go to implementation")
    "gI" '(lsp-bridge-find-impl-other-window :which-key "implementation (other window)")
    "gr" '(lsp-bridge-find-references :which-key "find references")
    "gh" '(lsp-bridge-popup-documentation :which-key "hover documentation")
    "gs" '(lsp-bridge-workspace-list-symbols :which-key "workspace symbols")
    "gS" '(lsp-bridge-workspace-list-symbol-at-point :which-key "symbol at point"))

  ;; Diagnostic navigation
  (general-def
    :states 'normal
    :keymaps 'override
    "]d" '(lsp-bridge-diagnostic-jump-next :which-key "next diagnostic")
    "[d" '(lsp-bridge-diagnostic-jump-prev :which-key "previous diagnostic"))

  ;; LSP management (SPC l)
  (emacs-leader
    :states 'normal
    :keymaps 'override
    "lr" '(lsp-bridge-restart-process :which-key "restart LSP")
    "ll" '(lsp-bridge-mode :which-key "toggle LSP")
    "ls" '(lsp-bridge-peek-file-content-scroll-up :which-key "scroll peek up")
    "lt" '(lsp-bridge-peek-file-content-scroll-down :which-key "scroll peek down")))

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
