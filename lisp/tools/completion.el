;;; completion.el --- Completion stack bootstrap -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defconst tools-completion--config-root
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

(defconst tools-completion--straight-bootstrap-file
  (expand-file-name "straight/repos/straight.el/bootstrap.el"
                    tools-completion--config-root)
  "Path to the local straight.el bootstrap file.")

(defun tools-completion--bootstrap-straight ()
  "Enable local straight/use-package integration when available."
  (when (file-exists-p tools-completion--straight-bootstrap-file)
    (load tools-completion--straight-bootstrap-file nil 'nomessage))
  (require 'use-package))

(tools-completion--bootstrap-straight)

(use-package vertico
  :straight t
  :init
  (vertico-mode 1))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic)))

(use-package marginalia
  :straight t
  :config
  (marginalia-mode 1))

(use-package recentf
  :ensure nil
  :init
  (setq recentf-auto-cleanup 'never)
  :config
  (recentf-mode 1))

(use-package consult
  :straight t
  :commands (consult-buffer
             consult-find
             consult-line
             consult-recent-file
             consult-ripgrep))

(require 'tools-actions "tools/actions")
(require 'tools-search "tools/search")

(provide 'tools-completion)
;;; completion.el ends here
