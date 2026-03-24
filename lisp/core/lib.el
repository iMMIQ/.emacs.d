;;; lib.el --- Small helpers for core startup -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun core-lib-provide-placeholder (feature)
  "Provide FEATURE as a transitional placeholder."
  (unless (featurep feature)
    (provide feature)))

(defun core-lib-provide-placeholders (features)
  "Provide each feature in FEATURES as a transitional placeholder."
  (dolist (feature features)
    (core-lib-provide-placeholder feature)))

(provide 'core-lib)
;;; lib.el ends here
