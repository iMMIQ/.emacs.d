;;; search.el --- Search commands for the minibuffer stack -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'use-package)

(use-package consult
  :straight t
  :commands (consult-line consult-ripgrep))

(provide 'tools-search)
;;; search.el ends here
