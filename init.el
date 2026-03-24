;;; init.el --- Startup entry point -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(setq load-prefer-newer t)

;; Active configuration lives under lisp/. The legacy config/ tree is
;; kept in the repo as inactive reference material and is not loaded.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'core-paths "core/paths")
(require 'core-bootstrap "core/bootstrap")
(require 'core-performance "core/performance")

(core-bootstrap-load-top-level-features)

(condition-case err
    (require 'personal-chinese "personal/chinese")
  (error (message "Skipping chinese module: %s" err)))

(condition-case err
    (require 'personal-music "personal/music")
  (error (message "Skipping music module: %s" err)))

(provide 'init)
;;; init.el ends here
