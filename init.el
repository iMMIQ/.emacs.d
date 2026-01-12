;;; init.el --- Main Emacs configuration file

;;; Commentary:
;; Main configuration entry point with modular loading system.

;;; Code:

;; ====================
;; Performance Optimization
;; ====================

;; Increase garbage collection threshold during startup for faster boot
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Reset GC after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216  ; 16MB
                  gc-cons-percentage 0.1)))

;; Increase file read limit for better LSP performance
(setq read-process-output-max (* 1024 1024))  ; 1MB

;; Reduce file name handlers overhead
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist file-name-handler-alist-original)))

;; Disable unnecessary features for performance
(setq inhibit-compacting-font-caches t)

;; Native compilation settings (Emacs 28+)
(when (featurep 'native-compile)
  (setq native-comp-deferred-compilation t
        native-comp-async-report-warnings-errors nil
        native-comp-jit-compilation t))

;; ====================
;; Package Management
;; ====================

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use straight for use-package
(straight-use-package 'use-package)
(setq use-package-compute-statistics t
      straight-vc-git-default-clone-depth 1
      package-enable-at-startup nil)

;; ====================
;; Configuration Loader
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
;; Core Modules (Always Load)
;; ====================

(use-package emacs
  :ensure nil
  :config
  (load-config-file "config/core/editor.el")
  (load-config-file "config/core/evil.el")
  (load-config-file "config/core/completion.el")
  (load-config-file "config/core/keybindings.el"))

;; ====================
;; Tool Modules
;; ====================

(use-package emacs
  :ensure nil
  :config
  (load-config-file "config/tools/vcs.el")
  (load-config-file "config/tools/project.el")
  (load-config-file "config/tools/eshell.el")
  (load-config-file "config/tools/search.el"))

;; ====================
;; Language Modules
;; ====================

(use-package emacs
  :ensure nil
  :config
  (load-config-file "config/lang/lsp.el")
  (load-config-file "config/lang/python.el")
  (load-config-file "config/lang/rust.el")
  (load-config-file "config/lang/verilog.el"))

;; ====================
;; UI Modules (Theme and Dashboard for both GUI and terminal)
;; ====================

(use-package emacs
  :ensure nil
  :config
  (load-config-file "config/ui/theme.el")
  (load-config-file "config/ui/themes.el"))

;; ====================
;; UI Modules (GUI only)
;; ====================

(use-package emacs
  :ensure nil
  :if (display-graphic-p)
  :config
  (load-config-file "config/ui/treemacs.el"))

;; ====================
;; Personal Modules
;; ====================

(use-package emacs
  :ensure nil
  :config
  (load-config-file "config/personal/chinese.el")
  (load-config-file "config/personal/music.el"))

;; ====================
;; Basic Settings
;; ====================

;; Enable xterm mouse mode in terminal
(unless (display-graphic-p)
  (xterm-mouse-mode 1))

;; ====================
;; Initialization Complete
;; ====================

(message "Emacs configuration loaded successfully!")

(provide 'init)
;;; init.el ends here
