;;; display.el --- Display shell defaults -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(provide 'ui-display)
;;; display.el ends here
