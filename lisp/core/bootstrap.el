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

(defconst core-bootstrap-feature-paths
  '((editor-evil . "editor/evil")
    (editor-keys . "editor/keys")
    (ui-theme . "ui/theme")
    (tools-completion . "tools/completion"))
  "Explicit layered file paths for top-level bootstrap features.")

(defun core-bootstrap-feature-path (feature)
  "Return the layered file path mapped for FEATURE, or nil."
  (alist-get feature core-bootstrap-feature-paths))

(defun core-bootstrap-apply-top-level-feature (feature)
  "Apply explicit startup behavior for top-level FEATURE."
  (when (and (eq feature 'ui-theme)
             (fboundp 'ui-theme-apply))
    (ui-theme-apply)))

(defun core-bootstrap-load-top-level-features ()
  "Load top-level modules, tolerating modules not implemented yet."
  (dolist (feature core-bootstrap-top-level-features)
    (core-lib-require-feature feature (core-bootstrap-feature-path feature))
    (core-bootstrap-apply-top-level-feature feature)))

(provide 'core-bootstrap)
;;; bootstrap.el ends here
