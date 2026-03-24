;;; paths.el --- Core path setup -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defconst core-paths-root-dir user-emacs-directory
  "Root directory for the Emacs configuration.")

(defconst core-paths-lisp-dir
  (expand-file-name "lisp" core-paths-root-dir)
  "Root directory for Lisp modules.")

(defconst core-paths-core-dir
  (expand-file-name "core" core-paths-lisp-dir)
  "Directory that contains core modules.")

(dolist (dir (list core-paths-lisp-dir core-paths-core-dir))
  (add-to-list 'load-path dir))

(provide 'core-paths)
;;; paths.el ends here
