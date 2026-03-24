;;; actions.el --- Action-oriented minibuffer tools -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'use-package)

(use-package embark
  :straight t
  :commands (embark-act))

(provide 'tools-actions)
;;; actions.el ends here
