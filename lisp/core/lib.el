;;; lib.el --- Small helpers for core startup -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun core-lib-require-feature (feature &optional path)
  "Require FEATURE from PATH when available, else provide a placeholder."
  (or (require feature nil t)
      (and path (require feature path t))
      (provide feature)))

(provide 'core-lib)
;;; lib.el ends here
