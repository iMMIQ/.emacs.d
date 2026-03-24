;;; music.el --- Optional music playback support -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(when (require 'use-package nil t)
  (condition-case err
      (eval
       '(use-package emms
          :straight t
          :defer t))
    (error
     (message "Skipping emms personal setup: %s" err))))

(provide 'personal-music)
;;; music.el ends here
