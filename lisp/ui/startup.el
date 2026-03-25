;;; startup.el --- Startup UI defaults -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(declare-function ui-icon-capable-p "ui/theme")

(defvar ui-startup--dashboard-hook-installed nil
  "Track whether `dashboard-setup-startup-hook' has already run.")

(defun ui-startup--icon-capable-p ()
  "Return non-nil when icon-capable UI can be used safely."
  (condition-case nil
      (and (fboundp 'ui-icon-capable-p)
           (ui-icon-capable-p))
    (error nil)))

(defun ui-startup-apply ()
  "Apply startup UI defaults."
  (setq inhibit-startup-screen t)
  (when (condition-case nil
            (require 'dashboard nil t)
          (error nil))
    (condition-case nil
        (progn
          (setq dashboard-banner-logo-title "Emacs"
                dashboard-startup-banner 'official
                dashboard-center-content t
                dashboard-show-shortcuts nil
                dashboard-footer-messages nil
                dashboard-set-heading-icons (ui-startup--icon-capable-p)
                dashboard-set-file-icons (ui-startup--icon-capable-p)
                dashboard-items '((recents . 6)
                                  (projects . 5)
                                  (bookmarks . 4)))
          (unless ui-startup--dashboard-hook-installed
            (dashboard-setup-startup-hook)
            (setq ui-startup--dashboard-hook-installed t)))
      (error
       (setq dashboard-set-heading-icons nil
             dashboard-set-file-icons nil))))
  (unless (ui-startup--icon-capable-p)
    (setq dashboard-set-heading-icons nil
          dashboard-set-file-icons nil)))

(provide 'ui-startup)
;;; startup.el ends here
