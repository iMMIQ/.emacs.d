;;; tree.el --- Optional tree tooling -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun tools-tree-toggle ()
  "Load tree tooling on demand and open the project tree."
  (interactive)
  (require 'tools-tree "tools/tree")
  (treemacs))

(eval
 (if (featurep 'straight)
     '(progn
        (require 'use-package)
        (use-package treemacs
          :straight t
          :commands (treemacs treemacs-select-window)
          :config
          (setq treemacs-width 32)
          (treemacs-follow-mode 1)
          (treemacs-filewatch-mode 1)
          (when (require 'doom-themes nil t)
            (when (fboundp 'doom-themes-treemacs-config)
              (doom-themes-treemacs-config)))))
   '(when (require 'use-package nil t)
      (use-package treemacs
        :commands (treemacs treemacs-select-window)
        :config
        (setq treemacs-width 32)
        (treemacs-follow-mode 1)
        (treemacs-filewatch-mode 1)
        (when (require 'doom-themes nil t)
          (when (fboundp 'doom-themes-treemacs-config)
            (doom-themes-treemacs-config)))))))

(provide 'tools-tree)
;;; tree.el ends here
