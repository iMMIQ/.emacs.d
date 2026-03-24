;;; git.el --- Git tooling bootstrap -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defconst tools-git--config-root
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

(defconst tools-git--straight-bootstrap-file
  (expand-file-name "straight/repos/straight.el/bootstrap.el"
                    tools-git--config-root)
  "Path to the local straight.el bootstrap file.")

(defun tools-git--bootstrap-straight ()
  "Enable local straight/use-package integration when available."
  (when (file-exists-p tools-git--straight-bootstrap-file)
    (load tools-git--straight-bootstrap-file nil 'nomessage))
  (require 'use-package))

(tools-git--bootstrap-straight)

(use-package magit
  :straight t
  :commands (magit-status))

(use-package diff-hl
  :straight t
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode 1))

(provide 'tools-git)
;;; git.el ends here
