;;; evil.el --- Evil editing bootstrap -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defconst editor-evil--straight-build-root
  (expand-file-name "straight/build" user-emacs-directory)
  "Directory containing locally built package artifacts.")

(defun editor-evil--add-package-to-load-path (package)
  "Add PACKAGE's straight build directory to `load-path' when present."
  (let ((dir (expand-file-name package editor-evil--straight-build-root)))
    (when (file-directory-p dir)
      (add-to-list 'load-path dir))))

(dolist (package '("goto-chg" "evil" "general" "which-key"))
  ;; Task 2 only needs already-built editor packages to be loadable.
  (editor-evil--add-package-to-load-path package))

(require 'use-package)

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(provide 'editor-evil)
;;; evil.el ends here
