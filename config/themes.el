(defun themes/load-theme (theme)
  "Load a specified Emacs theme."
  ;; Convert the theme to its string name if it's a symbol.
  (let* ((theme-name (if (symbolp theme)
                         (symbol-name theme)
                       theme))
         ;; Remove the "-theme" suffix if it exists.
         (full-theme-name (if (string-suffix-p "-theme" theme-name)
                              (substring theme-name 0 (- (length theme-name) 6))
                            theme-name)))
    ;; Try to install and load the theme, and display a message if it fails.
    (if (straight-use-package (intern (concat full-theme-name "-theme")))
        (load-theme (intern full-theme-name) t)
      (message "Failed to load theme: %s" theme))))

;; Load the specified theme.
(themes/load-theme emacs-theme)

;; Disable the menu bar.
(menu-bar-mode -1)
;; Disable the tool bar.
(tool-bar-mode -1)
;; Disable the scroll bar.
(scroll-bar-mode -1)
;; Set the frame title format to nil.
(setq frame-title-format nil)
;; Do not show trailing whitespace by default.
(setq-default show-trailing-whitespace nil)
;; Disable the startup screen.
(setq inhibit-startup-screen t)
;; Enable global display of line numbers.
(global-display-line-numbers-mode 1)
;; Enable line numbers in programming modes.
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Install and set up the Doom modeline.
(straight-use-package 'doom-modeline)
(require 'doom-modeline)
(setq doom-modeline-height 25
      doom-modeline-bar-width 3
      doom-modeline-buffer-file-name-style 'truncate-upto-project
      doom-modeline-icon t
      doom-modeline-major-mode-icon t
      doom-modeline-major-mode-color-icon t
      doom-modeline-minor-modes nil
      doom-modeline-enable-word-count t
      doom-modeline-buffer-encoding nil
      doom-modeline-indent-info nil)
(add-hook 'after-init-hook #'doom-modeline-mode)

;; Set the font for Doom Emacs
(setq doom-font (font-spec :family "Source Code Pro" :size 14)
      doom-variable-pitch-font (font-spec :family "Noto Sans" :size 13)
      doom-unicode-font (font-spec :family "Noto Sans Mono" :size 13)
      doom-big-font (font-spec :family "Source Code Pro" :size 20))

;; Use all-the-icons package for additional icons
(if (display-graphic-p)
  (progn
    (straight-use-package 'all-the-icons)
    (require 'all-the-icons)))

;; Install and set up the Winum package for window management.
(straight-use-package 'winum)
(require 'winum)
(winum-mode)

