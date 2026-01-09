;;; completion.el --- Ivy and Counsel configuration

;;; Commentary:
;; Completion framework configuration using Ivy, Counsel, and related packages.

;;; Code:

;; ====================
;; Ivy Core
;; ====================

(use-package ivy
  :straight t
  :diminish ivy-mode
  :init
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        enable-recursive-minibuffers t
        ivy-initial-inputs-alist nil)
  :config
  (ivy-mode 1))

;; ====================
;; Counsel
;; ====================

(use-package counsel
  :straight t
  :after ivy
  :config
  (setq counsel-find-file-ignore-regexp
        "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)")

  ;; Add additional file actions
  (dolist (fn '(counsel-rg counsel-find-file))
    (ivy-add-actions
     fn '(("p" (lambda (path) (with-ivy-window (insert (file-relative-name path default-directory))))
           "Insert relative path")
          ("P" (lambda (path) (with-ivy-window (insert path)))
           "Insert absolute path")
          ("l" (lambda (path) (with-ivy-window (insert (format "[[./%s]]" (file-relative-name path default-directory)))))
           "Insert relative org-link")
          ("L" (lambda (path) (with-ivy-window (insert (format "[[%s]]" path))))
           "Insert absolute org-link"))))

  (ivy-add-actions 'counsel-file-jump (plist-get ivy--actions-list 'counsel-find-file))

  ;; Custom file-jump using fd or ripgrep
  (defvar my/ignored-directories '(".git" "node_modules" "build")
    "List of directories to ignore during file search.")

  (defun my/counsel-file-jump-use-fd-or-rg (args)
    "Modify counsel-file-jump to use fd or ripgrep if available."
    (let* ((fd (executable-find "fd"))
           (rg (executable-find "rg"))
           (find-program (cond
                          (fd (append (list fd "--hidden" "--type" "file" "--type" "symlink" "--follow" "--color=never")
                                      (cl-loop for dir in my/ignored-directories
                                               collect "--exclude" collect dir)
                                      (when (eq system-type 'windows-nt)
                                        '("--path-separator=/"))))
                          (rg (append (list "rg" "--hidden" "--files" "--follow" "--color=never" "--no-messages")
                                      (cl-loop for dir in my/ignored-directories
                                               collect "--glob" collect (concat "!" dir))
                                      (when (eq system-type 'windows-nt)
                                        '("--path-separator=/"))))
                          (t (cons find-program args)))))
      (unless (listp args)
        (user-error "counsel-file-jump-args must be a list. Please customize accordingly."))
      (counsel--call
       find-program
       (lambda ()
         (goto-char (point-min))
         (let (files)
           (while (< (point) (point-max))
             (push (buffer-substring (line-beginning-position) (line-end-position))
                   files)
             (forward-line 1))
           (nreverse files))))))

  (advice-add 'counsel--find-return-list :override #'my/counsel-file-jump-use-fd-or-rg)
  (counsel-mode))

;; ====================
;; Ivy Rich
;; ====================

(use-package ivy-rich
  :straight t
  :after ivy
  :config
  (setq ivy-rich-parse-remote-buffer nil)
  (ivy-rich-mode 1))

(use-package all-the-icons-ivy-rich
  :straight t
  :if (display-graphic-p)
  :after ivy
  :config
  (all-the-icons-ivy-rich-mode 1))

;; ====================
;; Which Key
;; ====================

(use-package which-key
  :straight t
  :init
  (setq which-key-show-prefix 'SPC
        which-key-idle-delay 0.5)
  :config
  (which-key-mode))

;; ====================
;; General
;; ====================

(use-package general
  :straight t
  :config
  (which-key-add-key-based-replacements
    "SPC q" "Quit"
    "SPC f" "Files"
    "SPC b" "Buffers"
    "SPC w" "Windows"
    "SPC e" "Editing"
    "SPC s" "Search"
    "SPC h" "Help"
    "SPC g" "Version control"
    "SPC g v" "version control"
    "SPC '" "Eshell"
    "SPC SPC" "M-X")

  (general-create-definer emacs-leader
    :prefix "SPC")

  (general-create-definer emacs-local-leader
    :prefix "SPC m"))

;; ====================
;; Winum
;; ====================

(use-package winum
  :straight t
  :config
  (winum-mode))

;; ====================
;; Wgrep
;; ====================

(use-package wgrep
  :straight t
  :commands wgrep-change-to-wgrep-mode
  :config
  (setq wgrep-auto-save-buffer t))

(provide 'completion)
;;; completion.el ends here
