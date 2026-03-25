;;; modeline.el --- Lightweight modeline defaults -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(declare-function ui-icon-capable-p "ui/theme")

(defun ui-modeline--icon-capable-p ()
  "Return non-nil when icon-capable UI can be used safely."
  (condition-case nil
      (and (fboundp 'ui-icon-capable-p)
           (ui-icon-capable-p))
    (error nil)))

(defun ui-modeline-apply ()
  "Apply lightweight modeline defaults."
  (line-number-mode 1)
  (column-number-mode 1)
  (size-indication-mode 1)
  (when (condition-case nil
            (require 'doom-modeline nil t)
          (error nil))
    (setq doom-modeline-height 24
          doom-modeline-bar-width 3
          doom-modeline-buffer-file-name-style 'truncate-upto-project
          doom-modeline-minor-modes nil
          doom-modeline-buffer-encoding nil
          doom-modeline-indent-info nil
          doom-modeline-icon (ui-modeline--icon-capable-p))
    (doom-modeline-mode 1))
  (unless (ui-modeline--icon-capable-p)
    (setq doom-modeline-icon nil)))

(provide 'ui-modeline)
;;; modeline.el ends here
