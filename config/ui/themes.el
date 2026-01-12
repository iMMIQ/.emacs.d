;; Function to load a theme, installing it with straight.el if necessary
(defun themes/load-theme (theme)
  (let* ((theme-name (if (symbolp theme)
			 (symbol-name theme)
		       theme))
	 (full-theme-name (if (string-suffix-p "-theme" theme-name)
			      (substring theme-name 0 (- (length theme-name) 6))
			    theme-name)))
    (if (straight-use-package (intern (concat full-theme-name "-theme")))
	(load-theme (intern full-theme-name) t)
      (message "Failed to load theme: %s" theme))))

;; ====================
;; GUI-specific configuration
;; ====================

(when (display-graphic-p)
  ;; Load and configure 'all-the-icons' for graphical icons support
  (use-package all-the-icons
    :straight t
    :defer t
    :config
    (unless (member "all-the-icons" (font-family-list))
      (all-the-icons-install-fonts t)))

  ;; Set frame title format to nil
  (setq frame-title-format nil)

  ;; Start Emacs maximized
  (add-to-list 'initial-frame-alist '(fullscreen . maximized))

  ;; Set the default font
  (set-face-attribute 'default nil :font "JetBrains Mono 14"))

;; ====================
;; Common UI configuration (for both GUI and terminal)
;; ====================

;; Load and configure 'dashboard' for a custom Emacs startup screen
(use-package dashboard
  :straight t
  :init
  (setq dashboard-banner-logo-title "Welcome to GNU Emacs"
	dashboard-startup-banner (expand-file-name "image/GNUEmacs.png" user-emacs-directory)
	dashboard-set-heading-icons t
	dashboard-set-file-icons t
	dashboard-center-content t
	dashboard-items '((recents  . 5)
			  (bookmarks . 5)
			  (projects . 5)
			  (agenda . 5)
			  (registers . 5)))
  :config
  (dashboard-setup-startup-hook))

;; Load and configure 'doom-modeline' for an enhanced modeline
(use-package doom-modeline
  :straight t
  :defer t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-bar-width 3)
  (doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count t)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-indent-info nil)
  :config
  (when (display-graphic-p)
    (unless (member "Symbols Nerd Font Mono" (font-family-list))
      (nerd-icons-install-fonts t))))

;; Load the configured theme
(themes/load-theme emacs-theme)

;; Load and configure 'winum' for window number management
(use-package winum
  :straight t
  :defer t
  :config
  (winum-mode))

;; Disable menu bar, tool bar, and scroll bar
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Do not show trailing whitespace by default
(setq-default show-trailing-whitespace nil)

;; Disable the startup screen
(setq inhibit-startup-screen t)

;; Enable global line numbers
(global-display-line-numbers-mode 1)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(use-package rainbow-delimiters
  :straight t
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))
