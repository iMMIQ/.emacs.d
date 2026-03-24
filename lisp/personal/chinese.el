;;; chinese.el --- Optional Chinese input support -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(when (require 'use-package nil t)
  (condition-case err
      (eval
       '(use-package rime
          :straight t
          :defer t
          :init
          (setq default-input-method "rime")))
    (error
     (message "Skipping rime personal setup: %s" err))))

(provide 'personal-chinese)
;;; chinese.el ends here
