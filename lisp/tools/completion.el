;;; completion.el --- Completion stack bootstrap -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defconst tools-completion--straight-bootstrap-file
  (expand-file-name "straight/repos/straight.el/bootstrap.el"
                    user-emacs-directory)
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

(recentf-mode 1)

(require 'tools-actions "tools/actions")
(require 'tools-search "tools/search")

(provide 'tools-completion)
;;; completion.el ends here
