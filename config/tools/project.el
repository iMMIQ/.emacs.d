(use-package projectile
  :straight t
  :diminish projectile-mode
  :config
  (projectile-mode +1)
  (setq projectile-indexing-method 'alien)
  (setq projectile-completion-system 'ivy)
  (setq projectile-project-search-path '("~/projects/" "~/work/" ("~/github" . 1)))
  (add-hook 'project-find-functions #'project-projectile))

(use-package counsel-projectile
  :straight t
  :defer t
  :after (projectile counsel)
  :hook (projectile-mode . counsel-projectile-mode))

(defun set-projectile-keybindings ()
  "Set keybindings for projectile."
  (emacs-leader
    :states 'normal
    :keymaps 'override
    "p f" 'projectile-find-file
    "p F" 'projectile-find-file-in-known-projects
    "p g" 'projectile-find-file-dwim
    "p 4 f" 'projectile-find-file-other-window
    "p 4 g" 'projectile-find-file-dwim-other-window
    "p 5 f" 'projectile-find-file-other-frame
    "p 5 g" 'projectile-find-file-dwim-other-frame
    "p d" 'projectile-find-dir
    "p 4 d" 'projectile-find-dir-other-window
    "p 5 d" 'projectile-find-dir-other-frame
    "p T" 'projectile-find-test-file
    "p l" 'projectile-find-file-in-directory
    "p s g" 'projectile-grep
    "p s s" 'projectile-ag
    "p s r" 'projectile-ripgrep
    "p s x" 'projectile-find-tag
    "p v" 'projectile-vc
    "p V" 'projectile-browse-dirty-projects
    "p b" 'projectile-project-buffers
    "p 4 b" 'projectile-switch-to-buffer-other-window
    "p 5 b" 'projectile-switch-to-buffer-other-frame
    "p 4 C-o" 'projectile-display-buffer
    "p a" 'projectile-toggle-between-implementation-and-test
    "p 4 a" 'projectile-toggle-between-implementation-and-test-other-window
    "p 5 a" 'projectile-toggle-between-implementation-and-test-other-frame
    "p o" 'projectile-multi-occur
    "p r" 'projectile-replace
    "p i" 'projectile-invalidate-cache
    "p R" 'projectile-regenerate-tags
    "p j" 'projectile-find-tag
    "p k" 'projectile-kill-buffers
    "p D" 'projectile-dired
    "p 4 D" 'projectile-dired-other-window
    "p 5 D" 'projectile-dired-other-frame
    "p e" 'projectile-recentf
    "p <left>" 'projectile-previous-project-buffer
    "p <right>" 'projectile-next-project-buffer
    "p E" 'projectile-edit-dir-locals
    "p !" 'projectile-run-shell-command-in-root
    "p &" 'projectile-run-async-shell-command-in-root
    "p C" 'projectile-configure-project
    "p c" 'projectile-compile-project
    "p P" 'projectile-test-project
    "p t" 'projectile-toggle-between-implementation-and-test
    "p 4 t" 'projectile-toggle-between-implementation-and-test-other-window
    "p 5 t" 'projectile-toggle-between-implementation-and-test-other-frame
    "p z" 'projectile-cache-current-file
    "p p" 'projectile-switch-project
    "p q" 'projectile-switch-open-project
    "p S" 'projectile-save-project-buffers
    "p m" 'projectile-commander
    "p x e" 'projectile-run-eshell
    "p x i" 'projectile-run-ielm
    "p x t" 'projectile-run-ansi-term
    "p x s" 'projectile-run-shell
    "p x g" 'projectile-run-gdb
    "p x v" 'projectile-run-vterm
    "p ESC" 'projectile-previous-project-buffer))

(add-hook 'after-init-hook 'set-projectile-keybindings)

(which-key-add-key-based-replacements
  "SPC p" "Project"
  "SPC p s" "Search"
  "SPC p 4" "other-window"
  "SPC p 5" "other frame"
  "SPC p x" "Run")
