;;; lib.el --- Small helpers for core startup -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun core-lib-feature-path (feature)
  "Return the layered file path for FEATURE, or nil when unmapped."
  (let* ((name (symbol-name feature))
         (separator (string-search "-" name)))
    (when separator
      (concat (substring name 0 separator)
              "/"
              (substring name (1+ separator))))))

(defun core-lib-require-feature (feature)
  "Require FEATURE when available, else provide a placeholder."
  (or (require feature nil t)
      (let ((path (core-lib-feature-path feature)))
        (and path (require feature path t)))
      (provide feature)))

(provide 'core-lib)
;;; lib.el ends here
