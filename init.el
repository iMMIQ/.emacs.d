;; Define the bootstrap version
(defvar bootstrap-version)

;; Define the bootstrap file path
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))

  ;; If the bootstrap file does not exist, download and evaluate it
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))

  ;; Load the bootstrap file
  (load bootstrap-file nil 'nomessage))

;; Use straight to manage use-package
(straight-use-package 'use-package)

;; Enable statistics for use-package
(setq use-package-compute-statistics t)

;; Set the default clone depth for git to 1
(setq straight-vc-git-default-clone-depth 1)

;; Disable package.el initialization at startup
(setq package-enable-at-startup nil)

;; Set the theme to 'atom-one-dark'
(setq emacs-theme 'atom-one-dark)

(unless (display-graphic-p)
  (xterm-mouse-mode 1))

;; Load configuration files with conditional loading
;; Core configurations (always load)
(use-package emacs
  :ensure nil
  :config
  (load (expand-file-name "config/base.el" user-emacs-directory)))

(use-package emacs
  :ensure nil
  :config
  (load (expand-file-name "config/vcs.el" user-emacs-directory)))

(use-package emacs
  :ensure nil
  :config
  (load (expand-file-name "config/lsp.el" user-emacs-directory)))

(use-package emacs
  :ensure nil
  :config
  (load (expand-file-name "config/project.el" user-emacs-directory)))

(use-package emacs
  :ensure nil
  :config
  (load (expand-file-name "config/eshell.el" user-emacs-directory)))

(use-package emacs
  :ensure nil
  :config
  (load (expand-file-name "config/chinese.el" user-emacs-directory)))

;; Language-specific configurations
(use-package emacs
  :ensure nil
  :config
  (load (expand-file-name "config/python.el" user-emacs-directory)))

(use-package emacs
  :ensure nil
  :config
  (load (expand-file-name "config/rust.el" user-emacs-directory)))

(use-package emacs
  :ensure nil
  :config
  (load (expand-file-name "config/verilog.el" user-emacs-directory)))

;; GUI-specific configurations (only load in graphical mode)
(use-package emacs
  :ensure nil
  :if (display-graphic-p)
  :config
  (load (expand-file-name "config/themes.el" user-emacs-directory)))

(use-package emacs
  :ensure nil
  :if (display-graphic-p)
  :config
  (load (expand-file-name "config/treemacs.el" user-emacs-directory)))

;; Application configurations
(use-package emacs
  :ensure nil
  :config
  (load (expand-file-name "config/music.el" user-emacs-directory)))
