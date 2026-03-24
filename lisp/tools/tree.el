;;; tree.el --- Optional tree tooling -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defconst tools-tree--config-root
  (or (let ((origin (or load-file-name
                        byte-compile-current-file
                        buffer-file-name
                        default-directory)))
        (and origin
             (locate-dominating-file
              origin
              "straight/repos/straight.el/bootstrap.el")))
      user-emacs-directory)
  "Root directory for this config checkout.")

(defconst tools-tree--straight-bootstrap-file
  (expand-file-name "straight/repos/straight.el/bootstrap.el"
                    tools-tree--config-root)
  "Path to the local straight.el bootstrap file.")

(defun tools-tree--bootstrap-straight ()
  "Enable local straight/use-package integration when available."
  (when (file-exists-p tools-tree--straight-bootstrap-file)
    (load tools-tree--straight-bootstrap-file nil 'nomessage))
  (require 'use-package))

(tools-tree--bootstrap-straight)

(use-package treemacs
  :straight t
  :commands (treemacs treemacs-select-window))

(provide 'tools-tree)
;;; tree.el ends here
