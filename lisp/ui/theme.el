;;; theme.el --- Top-level UI theme entrypoint -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defconst ui-theme-default 'modus-operandi
  "Default theme loaded by the rebuilt UI shell.")

(require 'ui-startup "ui/startup")
(require 'ui-display "ui/display")
(require 'ui-modeline "ui/modeline")

(load-theme ui-theme-default t)

(provide 'ui-theme)
;;; theme.el ends here
