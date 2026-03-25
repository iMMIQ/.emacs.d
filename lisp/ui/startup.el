;;; startup.el --- Startup UI defaults -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(declare-function ui-icon-capable-p "ui/theme")

(defvar ui-startup--dashboard-hook-installed nil
  "Track whether `dashboard-setup-startup-hook' has already run.")

(defun ui-startup-apply ()
  "Apply startup UI defaults."
  (setq inhibit-startup-screen t)
  (when (condition-case nil
            (require 'dashboard nil t)
          (error nil))
    (setq dashboard-banner-logo-title "Emacs"
          dashboard-startup-banner 'official
          dashboard-center-content t
          dashboard-show-shortcuts nil
          dashboard-footer-messages nil
          dashboard-set-heading-icons (and (fboundp 'ui-icon-capable-p)
                                           (ui-icon-capable-p))
          dashboard-set-file-icons (and (fboundp 'ui-icon-capable-p)
                                        (ui-icon-capable-p))
          dashboard-items '((recents . 6)
                            (projects . 5)
                            (bookmarks . 4)))
    (unless ui-startup--dashboard-hook-installed
      (dashboard-setup-startup-hook)
      (setq ui-startup--dashboard-hook-installed t)))
  (unless (and (fboundp 'ui-icon-capable-p)
               (ui-icon-capable-p))
    (setq dashboard-set-heading-icons nil
          dashboard-set-file-icons nil)))

(provide 'ui-startup)
;;; startup.el ends here
