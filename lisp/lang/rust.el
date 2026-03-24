;;; rust.el --- Rust language support -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'lang-base "lang/base")

(use-package rustic
  :straight t
  :mode ("\\.rs\\'" . rustic-mode)
  :hook (rustic-mode . lsp-bridge-mode))

(provide 'lang-rust)
;;; rust.el ends here
