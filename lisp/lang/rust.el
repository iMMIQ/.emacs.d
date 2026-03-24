;;; rust.el --- Rust language support -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'lang-base "lang/base")

(defun lang-rust--enable-inlay-hints ()
  "Enable lsp-bridge inlay hints for the current Rust buffer."
  (setq-local lsp-bridge-enable-inlay-hint t))

(use-package rustic
  :straight t
  :preface
  (setq rustic-lsp-client nil)
  :mode ("\\.rs\\'" . rustic-mode)
  :hook ((rustic-mode . lang-rust--enable-inlay-hints)
         (rustic-mode . lsp-bridge-mode))
  :config
  (remove-hook 'rustic-mode-hook #'rustic-setup-lsp)
  (remove-hook 'rustic-mode-hook #'flycheck-mode)
  (remove-hook 'rustic-mode-hook #'flymake-mode-off)
  (setq rustic-indent-method-chain t
        rustic-babel-format-src-block nil
        rustic-format-trigger nil))

(provide 'lang-rust)
;;; rust.el ends here
