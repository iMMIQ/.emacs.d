;;; music.el --- Optional music playback support -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(when (require 'use-package nil t)
  (use-package emms
    :straight t
    :defer t))

(provide 'personal-music)
;;; music.el ends here
