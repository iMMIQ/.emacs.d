;; Load and configure 'ivy' package for completion and narrowing
(use-package ivy
  :straight t
  :diminish (ivy-mode)
  :init
  (setq ivy-use-virtual-buffers t           ;; Enable virtual buffers
        ivy-count-format "(%d/%d) "         ;; Display completion count
        enable-recursive-minibuffers t      ;; Allow recursive minibuffers
        ivy-initial-inputs-alist nil)       ;; Disable initial inputs
  :config
  (ivy-mode 1))                             ;; Enable ivy mode

;; Load 'counsel' for additional Ivy-based commands
(use-package counsel
  :straight t
  :after ivy
  :config
  (setq counsel-find-file-ignore-regexp     ;; Ignore specific files
        "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)")

  ;; Add additional actions to specific commands
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

  ;; Reuse these actions in counsel-file-jump
  (ivy-add-actions 'counsel-file-jump (plist-get ivy--actions-list 'counsel-find-file))

  ;; Custom function to modify file-jump using fd or ripgrep
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

  (counsel-mode))                          ;; Enable counsel mode

;; Enhance Ivy with icons
(use-package all-the-icons-ivy-rich
  :straight t
  :if (display-graphic-p)
  :after ivy
  :config
  (all-the-icons-ivy-rich-mode 1))         ;; Enable all-the-icons-ivy-rich mode

;; Enhance Ivy with additional info
(use-package ivy-rich
  :straight t
  :after ivy
  :config
  (setq ivy-rich-parse-remote-buffer nil)   ;; Disable remote buffer parsing
  (ivy-rich-mode 1))                        ;; Enable ivy-rich mode

;; Package for editing search results
(use-package wgrep
  :straight t
  :commands wgrep-change-to-wgrep-mode
  :config
  (setq wgrep-auto-save-buffer t))          ;; Auto-save wgrep changes

(defun set-search-keybindings ()
  "Set keybindings for search commands using both Ivy and Grep interfaces."
  (emacs-leader
    :states 'normal
    :keymaps 'override
    "sg" 'counsel-rg                  ;; s + g for ripgrep search
    "ss" 'swiper                      ;; s + s for Swiper (search in current buffer)
    "sc" 'counsel-git                 ;; s + c for git file search
    "sd" 'counsel-git-grep            ;; s + d for git grep in repository
    "sp" 'grep-or-fd-ivy              ;; s + p for directory search using Ivy
    "sf" 'grep-or-fd))                ;; s + f for directory search using Grep

(add-hook 'after-init-hook 'set-search-keybindings)

(defun build-search-command (initial-input exclude-dirs)
  "Build the search command using available tools (rg, fd, or grep) and exclude specified directories."
  (let* ((fd (executable-find "fd"))
         (rg (executable-find "rg"))
         (grep (executable-find "grep")))
    (cond
     (rg (concat rg " --hidden --files --no-messages --follow"
                 (mapconcat (lambda (dir) (format " --glob '!%s'" dir)) exclude-dirs "")
                 " | xargs rg --color=never -nH --no-heading -e "
                 (shell-quote-argument initial-input)))
     (fd (concat fd " --hidden --type f --follow"
                 (mapconcat (lambda (dir) (format " --exclude %s" dir)) exclude-dirs "")
                 " | xargs grep -nH -e "
                 (shell-quote-argument initial-input)))
     (grep (concat "find . -type f"
                   (mapconcat (lambda (dir) (format " -not -path './%s/*'" dir)) exclude-dirs "")
                   " | xargs grep -nH -e "
                   (shell-quote-argument initial-input)))
     (t (error "No grep, fd, or rg found on system")))))

(defun grep-or-fd-ivy (initial-input)
  "Search for a string in all files under the current directory using grep, fd, or rg with Ivy."
  (interactive "sSearch for: ")
  (let* ((default-directory (read-directory-name "Directory: "))
         (exclude-dirs '("node_modules" "build" ".git"))
         (search-cmd (build-search-command initial-input exclude-dirs)))
    (counsel--async-command search-cmd)
    (ivy-read (format "Results for \"%s\":" initial-input)
	      #'(lambda (str) (split-string (buffer-string) "\n" t))
              :initial-input initial-input
              :dynamic-collection t
              :action #'counsel-git-grep-action
              :caller 'grep-or-fd-ivy)))

(defun grep-or-fd (initial-input)
  "Search for a string in all files under the current directory using grep, fd, or rg with Grep."
  (interactive "sSearch for: ")
  (let* ((default-directory (read-directory-name "Directory: "))
         (exclude-dirs '("node_modules" "build" ".git"))
         (search-cmd (build-search-command initial-input exclude-dirs)))
    (compilation-start search-cmd 'grep-mode)))
