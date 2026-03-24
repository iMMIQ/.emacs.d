;;; chinese.el --- Optional Chinese input support -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(when (require 'use-package nil t)
  (use-package rime
    :straight t
    :defer t
    :init
    (setq default-input-method "rime")))

(provide 'personal-chinese)
;;; chinese.el ends here
