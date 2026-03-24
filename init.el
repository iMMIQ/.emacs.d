;;; init.el --- Startup entry point -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(setq load-prefer-newer t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'core-paths "core/paths")
(require 'core-bootstrap "core/bootstrap")
(require 'core-performance "core/performance")

(core-bootstrap-load-top-level-features)

(provide 'init)
;;; init.el ends here
