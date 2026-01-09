;;; search.el --- Search tools configuration

;;; Commentary:
;; Search functionality configuration (keybindings are in keybindings.el).

;;; Code:

;; ====================
;; Search Functions
;; ====================

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

(provide 'search)
;;; search.el ends here
