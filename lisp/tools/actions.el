;;; actions.el --- Action-oriented minibuffer tools -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'use-package)

(use-package consult
  :straight t
  :commands (consult-buffer consult-find consult-recent-file))

(use-package embark
  :straight t
  :commands (embark-act))

(provide 'tools-actions)
;;; actions.el ends here
