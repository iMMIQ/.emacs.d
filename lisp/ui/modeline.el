;;; modeline.el --- Lightweight modeline defaults -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun ui-modeline-apply ()
  "Apply lightweight modeline defaults."
  (line-number-mode 1)
  (column-number-mode 1)
  (size-indication-mode 1))

(provide 'ui-modeline)
;;; modeline.el ends here
