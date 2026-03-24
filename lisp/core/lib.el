;;; lib.el --- Small helpers for core startup -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun core-lib-require-feature (feature)
  "Require FEATURE when available, else provide a placeholder."
  (or (require feature nil t)
      (provide feature)))

(provide 'core-lib)
;;; lib.el ends here
