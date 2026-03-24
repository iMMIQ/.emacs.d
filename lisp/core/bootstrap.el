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

(core-lib-provide-placeholders core-bootstrap-top-level-features)

(provide 'core-bootstrap)
;;; bootstrap.el ends here
