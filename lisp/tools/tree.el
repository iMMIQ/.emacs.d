;;; tree.el --- Optional tree tooling -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun tools-tree--ensure-loaded ()
  "Load tree tooling and return non-nil when the entrypoint is available."
  (condition-case err
      (progn
        (require 'tools-tree "tools/tree")
        t)
    (error
     (message "Treemacs is unavailable: %s" err)
     nil)))

(defun tools-tree-toggle ()
  "Load tree tooling on demand and open the project tree."
  (interactive)
  (when (tools-tree--ensure-loaded)
    (unless (fboundp 'treemacs)
      (require 'treemacs nil t))
    (if (fboundp 'treemacs)
        (treemacs)
      (message "Treemacs is unavailable"))))

(condition-case err
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
  (error
   (message "Tree tooling setup failed: %s" err)))

(provide 'tools-tree)
;;; tree.el ends here
