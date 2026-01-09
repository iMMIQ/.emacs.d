(defvar +eshell-aliases
  '(
    ;; Custom aliases for eshell commands
    ("q"  "exit")                   ;; Quit eshell
    ("f"  "find-file $1")            ;; Open a file
    ("ff" "find-file-other-window $1") ;; Open a file in another window
    ("d"  "dired $1")                ;; Open directory in dired
    ("rg" "rg --color=always $*")    ;; Run ripgrep with color
    ("ll" "ls --color=auto -lah $*") ;; List directory contents with details
    ("git" "git --no-pager $*")      ;; Run git without pager
    ("gg" "magit-status")            ;; Open magit status
    ("cdp" "cd-to-project")          ;; Change directory to project root
    ("clear" "clear-scrollback")     ;; Clear eshell scrollback
    ("up" "eshell-up $1")            ;; Move up directories
    ("pk" "eshell-up-peek $1")       ;; Peek at upper directory without changing
    ))

(defun my-eshell-prompt ()
  "Custom eshell prompt function."
  ;; Create a prompt showing username, system name, and current directory
  (concat
   (propertize (user-login-name) 'face 'font-lock-keyword-face) 
   "@"
   (propertize (system-name) 'face 'font-lock-keyword-face)
   ":"
   (propertize (eshell/pwd) 'face 'font-lock-string-face) 
   ;; Use "#" if user is root, otherwise "$"
   (if (= (user-uid) 0) 
       " # " 
     " $ ")))

(defun is-wsl-system-p ()
  "Check if the system is running on WSL."
  (and (eq system-type 'gnu/linux)
       (string-match "microsoft" (shell-command-to-string "uname -r"))))

(defun remove-mnt-paths (path)
  "Remove /mnt paths (Windows paths) from the environment PATH."
  (string-join
   (seq-remove (lambda (p) (string-prefix-p "/mnt/" p))
	       (split-string path path-separator))
   path-separator))

(defun setup-wsl-path ()
  "Set up PATH for WSL to exclude Windows paths."
  (when (is-wsl-system-p)
    (let ((new-path (remove-mnt-paths (getenv "PATH"))))
      (setenv "PATH" new-path)
      (setq eshell-path-env new-path))))

(use-package highlight-quoted
  :straight t
  :config (highlight-quoted-mode)) ;; Enable highlighting for quoted expressions

(use-package eshell
  :defer t
  :init
  (setup-wsl-path) ;; Set up WSL-specific PATH
  :hook ((eshell-mode . (lambda () (set-buffer-modified-p nil)))
         (eshell-mode . (lambda () (set-window-fringes nil 0 0))) ;; Remove fringes in eshell
         (eshell-mode . (lambda () (visual-line-mode +1))) ;; Enable line wrapping
         (eshell-mode . (lambda () (setq hscroll-margin 0))) ;; Disable horizontal scrolling
         (eshell-mode . (lambda ()
                          ;; Custom imenu entries
                          (setq imenu-generic-expression
				`((,(propertize "λ" 'face 'eshell-prompt)
                                   ,(concat eshell-prompt-regexp "\\(.*\\)") 1))))))
  :config
  ;; Eshell configurations
  (setq eshell-scroll-to-bottom-on-input 'all
        eshell-scroll-to-bottom-on-output 'all
        eshell-kill-processes-on-exit t
        eshell-hist-ignoredups t
        eshell-input-filter 'eshell-input-filter-initial-space
        eshell-prompt-regexp "^[^#$\n]* [#$λ] "
        eshell-prompt-function #'my-eshell-prompt
        eshell-glob-case-insensitive t
        eshell-error-if-no-glob t)

  (advice-add #'eshell-write-aliases-list :override #'ignore) ;; Ignore writing aliases list

  (add-to-list 'eshell-modules-list 'eshell-tramp) ;; Enable TRAMP in eshell

  (with-eval-after-load 'em-term
    ;; Make certain commands open in a terminal
    (dolist (cmd '("tmux" "htop" "vim" "nvim" "ncmpcpp"))
      (add-to-list 'eshell-visual-commands cmd))))

(use-package eshell-up
  :straight t
  :after eshell
  :commands eshell-up eshell-up-peek) ;; Commands to move up directories

(use-package eshell-syntax-highlighting
  :straight t
  :after eshell
  :config
  (eshell-syntax-highlighting-global-mode +1) ;; Enable syntax highlighting
  (add-hook 'eshell-syntax-highlighting-elisp-buffer-setup-hook #'highlight-quoted-mode)) ;; Highlight quoted code

(use-package eshell-z
  :straight t
  :after eshell
  :config
  ;; Configure eshell-z, which tracks directory usage frequency
  (unless (file-exists-p eshell-z-freq-dir-hash-table-file-name)
    (setq eshell-z-freq-dir-hash-table-file-name
          (expand-file-name "z" eshell-directory-name))))

(use-package eshell-did-you-mean
  :straight t
  :after eshell
  :config
  ;; Improve the logic for "Did you mean" suggestions
  (defun my-eshell-did-you-mean-get-all-commands-from-path ()
    (unless eshell-did-you-mean--all-commands
      (setq eshell-did-you-mean--all-commands
            (eshell-parse-commands eshell-path-env))))

  (defun eshell-parse-commands (paths)
    "Parse the PATH and get all available commands."
    (let ((command-list '()))
      (dolist (path (split-string paths path-separator))
        (when (file-exists-p path)
          (dolist (file (directory-files path t "^[^.]" t))
            (when (and (file-executable-p file)
                       (not (file-directory-p file)))
              (push (file-name-nondirectory file) command-list)))))
      (delete-dups command-list)))

  ;; Override the original method to improve performance
  (advice-add 'eshell-did-you-mean--get-all-commands :override #'my-eshell-did-you-mean-get-all-commands-from-path)
  (eshell-did-you-mean-setup))

(use-package em-alias
  :after eshell
  :config
  ;; Add custom aliases to eshell
  (setq eshell-command-aliases-list
        (append eshell-command-aliases-list
                +eshell-aliases)))
