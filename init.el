;;; init.el --- Startup entry point -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'core-paths "core/paths")
(require 'core-bootstrap "core/bootstrap")
(require 'core-performance "core/performance")

(dolist (feature '(editor-evil
                   editor-keys
                   ui-theme
                   tools-completion))
  (require feature))

(provide 'init)
;;; init.el ends here
