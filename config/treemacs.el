(use-package treemacs
  :straight t
  :defer t
  :config
  (defun my/disable-linum-in-treemacs ()
    "Disable line numbers in Treemacs buffer."
    (when (derived-mode-p 'treemacs-mode)
      (display-line-numbers-mode -1)))

  (add-hook 'treemacs-mode-hook #'my/disable-linum-in-treemacs)

  (treemacs-follow-mode t)
  (treemacs-tag-follow-mode t)
  (treemacs-fringe-indicator-mode t)
  (treemacs-git-mode 'deferred)
  (treemacs-git-commit-diff-mode t)
  (treemacs-project-follow-mode t)
  (treemacs-indent-guide-mode 'line)
  (with-eval-after-load 'treemacs
    (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action))

  (use-package treemacs-evil
    :straight t
    :defer t
    :after (treemacs evil))

  (use-package treemacs-projectile
    :straight t
    :defer t
    :after (treemacs projectile))

  (use-package treemacs-magit
    :straight t
    :defer t
    :after (treemacs magit))

  (use-package treemacs-all-the-icons
    :straight t
    :defer t
    :after treemacs))


(defun set-treemacs-keybindings ()
  "Set keybindings for treemacs."
  (emacs-leader
    :states 'normal
    :keymaps 'override
    "0" 'treemacs-select-window))

(add-hook 'after-init-hook 'set-treemacs-keybindings)
