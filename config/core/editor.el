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
(setq-default bidi-display-reordering nil
              bidi-inhibit-bpa t)

;; Optimize rendering for large files
(setq-default redisplay-skip-fontification-on-input t
              jit-lock-defer-time 0
              jit-lock-context-time 0
              fast-but-imprecise-scrolling t
              scroll-conservatively 0
              scroll-margin 0
              scroll-preserve-screen-position t)

;; Reduce mode line updates
(setq-default mode-line-update-pct 100)

;; Optimize syntax highlighting for large files
(setq-default syntax-wholeline-max 10000)

;; Disable overstrike mode for better performance
(setq-default overwrite-mode nil)

;; ====================
;; Display Modes
;; ====================

;; Enable line numbers only in programming modes
(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook (lambda () (display-line-numbers-mode 1))))

(setq display-line-numbers-type 'relative
      display-line-numbers-width-start t)

;; Pixel scroll precision (only for GUI)
(when (display-graphic-p)
  (pixel-scroll-precision-mode t))

(auto-image-file-mode t)
(global-visual-line-mode t)
(blink-cursor-mode 0)  ; Disable cursor blinking for performance

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
