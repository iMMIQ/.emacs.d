;;; base.el --- Shared language tooling -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defconst lang-base--config-root
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

(defconst lang-base--straight-bootstrap-file
  (expand-file-name "straight/repos/straight.el/bootstrap.el"
                    lang-base--config-root)
  "Path to the local straight.el bootstrap file.")

(defun lang-base--bootstrap-straight ()
  "Enable local straight/use-package integration when available."
  (when (file-exists-p lang-base--straight-bootstrap-file)
    (load lang-base--straight-bootstrap-file nil 'nomessage))
  (require 'use-package))

(lang-base--bootstrap-straight)

(use-package yasnippet
  :straight t
  :commands (yas-minor-mode yas-expand-snippet))

(use-package lsp-bridge
  :straight (lsp-bridge :host github
                        :repo "manateelazycat/lsp-bridge"
                        :files ("*.el" "*.py" "acm" "core" "langserver"
                                "multiserver" "resources"))
  :init
  (setq lsp-bridge-enable-log nil
        lsp-bridge-enable-hover-diagnostic t
        lsp-bridge-enable-signature-help t
        lsp-bridge-completion-stop-when-inserting t)
  :commands (lsp-bridge-code-action
             lsp-bridge-diagnostic-jump-next
             lsp-bridge-diagnostic-jump-prev
             lsp-bridge-diagnostic-list
             lsp-bridge-mode
             lsp-bridge-rename
             lsp-bridge-restart-process))

(use-package apheleia
  :straight t
  :commands (apheleia-format))

(unless (display-graphic-p)
  (use-package popon
    :straight (popon :host nil
                     :repo "https://codeberg.org/akib/emacs-popon.git")
    :defer t)
  (use-package acm-terminal
    :straight (acm-terminal :host github :repo "twlz0ne/acm-terminal")
    :after acm))

(provide 'lang-base)
;;; base.el ends here
