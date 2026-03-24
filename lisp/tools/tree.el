;;; tree.el --- Optional tree tooling -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'use-package)

(eval
 (if (featurep 'straight)
     '(use-package treemacs
        :straight t
        :commands (treemacs treemacs-select-window))
   '(use-package treemacs
      :commands (treemacs treemacs-select-window))))

(provide 'tools-tree)
;;; tree.el ends here
