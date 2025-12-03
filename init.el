;;; init.el --- Main Emacs configuration file

;;; Commentary:
;; This is the main configuration file for Emacs.
;; It sets up package management and loads modular configuration files.

;;; Code:

;; ====================
;; Package Management
;; ====================

;; Define the bootstrap version
(defvar bootstrap-version)

;; Bootstrap straight.el package manager
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  
  ;; Download straight.el if not present
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  
  ;; Load straight.el
  (load bootstrap-file nil 'nomessage))

;; Use straight to manage use-package
(straight-use-package 'use-package)

;; Enable use-package statistics
(setq use-package-compute-statistics t)

;; Set default git clone depth for performance
(setq straight-vc-git-default-clone-depth 1)

;; Disable built-in package.el to avoid conflicts
(setq package-enable-at-startup nil)

;; ====================
;; Basic Settings
;; ====================

;; Set default theme
(setq emacs-theme 'atom-one-dark)

;; Enable xterm mouse mode in terminal
(unless (display-graphic-p)
  (xterm-mouse-mode 1))

;; ====================
;; Configuration Loading
;; ====================

(defun load-config-file (filename &optional condition)
  "Load configuration file FILENAME with optional CONDITION.
If CONDITION is non-nil, only load if CONDITION evaluates to true."
  (when (or (null condition) (eval condition))
    (let ((filepath (expand-file-name filename user-emacs-directory)))
      (condition-case err
          (progn
            (load filepath nil 'nomessage)
            (message "Loaded config: %s" filename))
        (error
         (warn "Failed to load %s: %s" filename (error-message-string err)))))))

;; ====================
;; Core Configurations
;; ====================

;; Always load core configurations
(use-package emacs
  :ensure nil
  :config
  (progn
    (load-config-file "config/base.el")
    (load-config-file "config/vcs.el")
    (load-config-file "config/lsp.el")
    (load-config-file "config/project.el")
    (load-config-file "config/eshell.el")
    (load-config-file "config/chinese.el")))

;; ====================
;; Language Configurations
;; ====================

(use-package emacs
  :ensure nil
  :config
  (progn
    (load-config-file "config/python.el")
    (load-config-file "config/rust.el")
    (load-config-file "config/verilog.el")))

;; ====================
;; GUI-specific Configurations
;; ====================

(use-package emacs
  :ensure nil
  :if (display-graphic-p)
  :config
  (progn
    (load-config-file "config/themes.el")
    (load-config-file "config/treemacs.el")))

;; ====================
;; Application Configurations
;; ====================

(use-package emacs
  :ensure nil
  :config
  (load-config-file "config/music.el"))

;; ====================
;; Initialization Complete
;; ====================

(message "Emacs configuration loaded successfully!")

(provide 'init)
;;; init.el ends here
