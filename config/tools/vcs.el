;; Load and configure 'magit' for Git integration
(use-package magit
  :straight t
  :defer t
  :init
  (setq magit-log-section-commit-count 20) ;; Show 20 commits in log sections
  (setq magit-diff-refine-hunk 'all)       ;; Highlight differences within lines
  :hook
  (magit-mode . display-line-numbers-mode) ;; Enable line numbers in magit mode
  (git-commit-mode . enable-flyspell-if-installed) ;; Enable flyspell in commit mode
  :config
  ;; Function to enable flyspell if it's installed
  (defun enable-flyspell-if-installed ()
    (when (require 'flyspell nil 'noerror)
      (flyspell-mode 1)))
  
  ;; Use 'fullframe' package if installed, otherwise configure fullscreen behavior manually
  (if (package-installed-p 'fullframe)
      (progn
	(use-package fullframe
	  :straight t
	  :config
	  (fullframe magit-status magit-mode-quit-window)))
    (progn
      (defadvice magit-status (around magit-fullscreen activate)
	(window-configuration-to-register :magit-fullscreen)
	ad-do-it
	(delete-other-windows))
      (defun magit-quit-session ()
	(interactive)
	(kill-buffer)
	(jump-to-register :magit-fullscreen))
      (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)))
  
  ;; Use 'magit-gitflow' if 'git-flow' executable is found
  (if (executable-find "git-flow")
      (progn
	(use-package magit-gitflow
	  :straight t
	  :hook (magit-mode . turn-on-magit-gitflow)))))

;; Load and configure 'diff-hl' for highlighting changes in buffers
(use-package diff-hl
  :straight t
  :defer t
  :hook
  (magit-post-refresh . diff-hl-magit-post-refresh) ;; Update diff-hl after magit refresh
  :config
  (global-diff-hl-mode)) ;; Enable diff-hl globally

;; Load and configure 'magit-todos' for displaying TODOs in magit
(use-package magit-todos
  :straight t
  :defer t
  :hook (magit-mode . magit-todos-mode))

;; Load and configure 'transient' for better popup management in magit
(use-package transient
  :straight t
  :defer t
  :init
  (setq transient-default-level 5)) ;; Set default transient level to 5

(emacs-leader
  :states 'normal
  :keymaps 'override
  ;; Magit keybindings
  "gs" 'magit-status
  "gb" 'magit-branch
  "gc" 'magit-commit
  "gd" 'magit-diff
  "gl" 'magit-log
  "gp" 'magit-push
  "gr" 'magit-pull
  "gf" 'magit-fetch
  "gm" 'magit-merge
  "gR" 'magit-rebase
  "gt" 'magit-tag
  "gS" 'magit-stage-file
  "gU" 'magit-unstage-file
  "gC" 'magit-checkout
  "gB" 'magit-blame-addition
  ;; Version control keybindings
  "gv+" 'vc-update
  "gv=" 'diff-hl-diff-goto-hunk
  "gvd" 'vc-dir
  "gve" 'vc-ediff
  "gvg" 'vc-annotate
  "gvi" 'vc-register
  "gvl" 'vc-print-log
  "gvr" 'vc-resolve-conflicts
  "gvu" 'vc-revert
  "gvv" 'vc-next-action
  "gvD" 'vc-root-diff
  "gvI" 'vc-ignore
  "gvL" 'vc-print-root-log
  )

