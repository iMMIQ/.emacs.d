;;; python.el --- Python language support -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'lang-base "lang/base")

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :hook (python-mode . lsp-bridge-mode))

(provide 'lang-python)
;;; python.el ends here
