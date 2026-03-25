;;; theme.el --- Top-level UI theme entrypoint -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'ui-startup "ui/startup")
(require 'ui-display "ui/display")
(require 'ui-modeline "ui/modeline")

(defconst ui-theme--straight-build-root
  (expand-file-name "straight/build" user-emacs-directory)
  "Root directory for already-built optional UI packages.")

(defconst ui-theme-default 'doom-one
  "Default theme loaded by the UI shell.")

(defun ui-theme--add-package-to-load-path (package)
  "Add PACKAGE under the straight build root to `load-path' when present."
  (let ((dir (expand-file-name package ui-theme--straight-build-root)))
    (when (file-directory-p dir)
      (add-to-list 'load-path dir))))

(defun ui-theme--prepare-optional-packages ()
  "Expose already-built optional UI packages without requiring `use-package'."
  (dolist (package '("doom-themes" "doom-modeline" "dashboard" "nerd-icons"))
    (ui-theme--add-package-to-load-path package)))

(defun ui-icon-capable-p ()
  "Return non-nil when nerd icons can be displayed in the current frame."
  (and (display-graphic-p)
       (require 'nerd-icons nil t)
       (or (member "Symbols Nerd Font Mono" (font-family-list))
           (member "Symbols Nerd Font" (font-family-list)))))

(defun ui-theme--load-default-theme ()
  "Load the configured default theme with a safe fallback."
  (mapc #'disable-theme (copy-sequence custom-enabled-themes))
  (unless (and (require 'doom-themes nil t)
               (condition-case nil
                   (progn
                     (load-theme ui-theme-default t)
                     t)
                 (error nil)))
    (ignore-errors (load-theme 'deeper-blue t))))

(defun ui-theme-apply ()
  "Apply the rebuilt UI shell defaults."
  (interactive)
  (ui-theme--prepare-optional-packages)
  (ui-startup-apply)
  (ui-display-apply)
  (ui-modeline-apply)
  (ui-theme--load-default-theme))

(provide 'ui-theme)
;;; theme.el ends here
