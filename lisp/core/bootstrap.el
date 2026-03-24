;;; bootstrap.el --- Core startup bootstrap -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'core-lib "core/lib")

(defconst core-bootstrap-top-level-features
  '(editor-evil
    editor-keys
    ui-theme
    tools-completion)
  "Top-level module groups required during the skeleton phase.")

(defun core-bootstrap-load-top-level-features ()
  "Load top-level modules, tolerating modules not implemented yet."
  (dolist (feature core-bootstrap-top-level-features)
    (core-lib-require-feature feature)))

(provide 'core-bootstrap)
;;; bootstrap.el ends here
