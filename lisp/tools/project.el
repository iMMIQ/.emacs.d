;;; project.el --- Native project helpers -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'project)

(defconst tools-project--config-root
  (or (let ((origin (or load-file-name
                        byte-compile-current-file
                        buffer-file-name
                        default-directory)))
        (and origin
             (locate-dominating-file
              origin
              "straight/repos/straight.el/bootstrap.el")))
      user-emacs-directory)
  "Root directory for this config checkout.")

(let ((consult-build-dir
       (expand-file-name "straight/build/consult" tools-project--config-root)))
  (when (file-directory-p consult-build-dir)
    (add-to-list 'load-path consult-build-dir)))

(autoload 'consult-ripgrep "consult" nil t)

(defun my/project-find-file ()
  "Find a file in the current project."
  (interactive)
  (call-interactively #'project-find-file))

(defun my/project-switch ()
  "Switch to another project."
  (interactive)
  (call-interactively #'project-switch-project))

(defun my/project-search ()
  "Search the current project."
  (interactive)
  (call-interactively #'consult-ripgrep))

(provide 'tools-project)
;;; project.el ends here
