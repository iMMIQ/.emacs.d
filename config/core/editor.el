;;; editor.el --- Basic editor settings

;;; Commentary:
;; Basic editor configuration including encoding, scrolling, and editing modes.

;;; Code:

;; ====================
;; Encoding Settings
;; ====================

(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)

;; ====================
;; Performance Settings
;; ====================

;; Disable bidirectional text reordering for performance
(setq-default bidi-display-reordering nil)

;; ====================
;; Display Modes
;; ====================

(pixel-scroll-precision-mode t)
(auto-image-file-mode t)
(global-visual-line-mode t)
(blink-cursor-mode t)
(global-display-line-numbers-mode t)

;; ====================
;; Editing Enhancement
;; ====================

(use-package smartparens
  :straight t
  :config
  (smartparens-global-mode t))

;; ====================
;; Utilities
;; ====================

(use-package restart-emacs
  :straight t)

(provide 'editor)
;;; editor.el ends here
