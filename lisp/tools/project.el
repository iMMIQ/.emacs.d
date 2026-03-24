;;; project.el --- Native project helpers -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'project)

(defun my/project-find-file ()
  "Find a file in the current project."
  (interactive)
  (call-interactively #'project-find-file))

(defun my/project-switch ()
  "Switch to another project."
  (interactive)
  (call-interactively #'project-switch-project))

(provide 'tools-project)
;;; project.el ends here
