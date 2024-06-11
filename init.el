;; Define the variable bootstrap-version
(defvar bootstrap-version)

;; Let-binding to define the bootstrap-file path and bootstrap-version
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        ;; Use the straight-base-dir if bound and true, otherwise use user-emacs-directory
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  ;; Check if the bootstrap-file does not exist
  (unless (file-exists-p bootstrap-file)
    ;; Retrieve the bootstrap script from the straight.el repository and evaluate it
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  ;; Load the bootstrap file without messages
  (load bootstrap-file nil 'nomessage))

;; Set the default clone depth for straight.el to 1
(setq straight-vc-git-default-clone-depth 1)

;; Disable the automatic package initialization at startup
(setq package-enable-at-startup nil)

;; Set the Emacs theme to atom-one-dark
(setq emacs-theme 'atom-one-dark)

;; Set the default font to JetBrains Mono with a size of 14
(set-face-attribute 'default nil :font "JetBrains Mono 14")

;; Load additional configuration files for themes and base settings
(load (expand-file-name "config/themes.el" user-emacs-directory))
(load (expand-file-name "config/base.el" user-emacs-directory))

