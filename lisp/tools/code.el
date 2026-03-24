;;; code.el --- Shared code action tooling -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defconst tools-code--config-root
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

(defconst tools-code--straight-bootstrap-file
  (expand-file-name "straight/repos/straight.el/bootstrap.el"
                    tools-code--config-root)
  "Path to the local straight.el bootstrap file.")

(defun tools-code--bootstrap-straight ()
  "Enable local straight/use-package integration when available."
  (when (file-exists-p tools-code--straight-bootstrap-file)
    (load tools-code--straight-bootstrap-file nil 'nomessage))
  (require 'use-package))

(tools-code--bootstrap-straight)

(use-package apheleia
  :straight t
  :commands (apheleia-format))

(use-package lsp-bridge
  :straight (lsp-bridge :host github
                        :repo "manateelazycat/lsp-bridge"
                        :files ("*.el" "*.py" "acm" "core" "langserver"
                                "multiserver" "resources"))
  :commands (lsp-bridge-code-action
             lsp-bridge-diagnostic-jump-next
             lsp-bridge-diagnostic-jump-prev
             lsp-bridge-diagnostic-list
             lsp-bridge-rename
             lsp-bridge-restart-process))

(provide 'tools-code)
;;; code.el ends here
