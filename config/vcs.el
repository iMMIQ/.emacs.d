(straight-use-package 'magit)
(require 'magit)

(setq magit-log-section-commit-count 20) ; Set the number of commits shown in Magit log sections.
(add-hook 'magit-mode-hook 'display-line-numbers-mode) ; Enable line numbers in Magit mode.
(setq magit-diff-refine-hunk 'all) ; Refine diff hunks to highlight word changes.

(defun enable-flyspell-if-installed ()
  "Enable flyspell mode if available."
  (when (require 'flyspell nil 'noerror)
    (flyspell-mode 1)))

;; Configure fullscreen behavior for Magit status buffer.
(if (package-installed-p 'fullframe)
    (progn
      (straight-use-package 'fullframe)
      (fullframe magit-status magit-mode-quit-window))
  (progn
    (defadvice magit-status (around magit-fullscreen activate)
      "Toggle fullscreen when opening Magit status buffer."
      (window-configuration-to-register :magit-fullscreen)
      ad-do-it
      (delete-other-windows))
    (defun magit-quit-session ()
      "Quit Magit session and restore previous window configuration."
      (interactive)
      (kill-buffer)
      (jump-to-register :magit-fullscreen))
    (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)))

(add-hook 'git-commit-mode-hook 'enable-flyspell-if-installed) ; Enable flyspell in git commit mode.
(provide 'init-magit)

(if (executable-find "git-glow")
    (progn
      (straight-use-package 'magit-gitflow)
      (require 'magit-gitflow)
      (add-hook 'magit-mode-hook 'turn-on-magit-gitflow)))

(straight-use-package 'diff-hl)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
(global-diff-hl-mode)

(straight-use-package 'magit-todos)
(magit-todos-mode)

(straight-use-package 'transient)
(setq transient-default-level 5)

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

