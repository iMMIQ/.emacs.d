;;; windows.el --- Window and buffer management configuration

;;; Commentary:
;; Window sizing, split behavior, and tab bar configuration.

;;; Code:

;; ====================
;; Golden Ratio - Automatic window resizing
;; ====================
(use-package golden-ratio
  :straight t
  :defer t
  :hook (after-init . golden-ratio-mode)
  :custom
  (golden-ratio-auto-scale t)
  (golden-ratio-mode-set-movement-keys nil)
  (golden-ratio-inhibit-functions
   '(golden-ratio--inhibit-dir
     golden-ratio--inhibit-fullscreen-p))
  :config
  ;; Inhibit golden-ratio in certain modes
  (defun golden-ratio--inhibit-fullscreen-p ()
    "Inhibit golden-ratio when in fullscreen."
    (when (bound-and-true-p treemacs-mode)
      t)))

;; ====================
;; Improved window split behavior
;; ====================
;; Prefer vertical splits
(setq split-width-threshold 160)
(setq split-height-threshold nil)

;; Always split window vertically (side-by-side) by default
(defun my/split-window-sensibly (&optional window)
  "Split WINDOW sensibly, preferring vertical splits."
  (let ((window (or window (selected-window))))
    (or (and (window-splittable window t)
	     ;; Split window vertically
	     (with-selected-window window
	       (split-window-right)))
	(and (window-splittable window)
	     (with-selected-window window
	       (split-window-below)))
	(and (eq window (frame-root-window window))
	     (not (window-minibuffer-p window))
	     ;; If we can't split, create a new frame
	     (let ((frame (make-frame)))
	       (select-window (frame-root-window frame))
	       frame)))))

(advice-add 'split-window-sensibly :override #'my/split-window-sensibly)

;; Make windmove work in all frames
(windmove-default-keybindings 'super)

;; ====================
;; Centaur Tabs - Beautiful tab bar
;; ====================
(use-package centaur-tabs
  :straight t
  :demand
  :config
  (centaur-tabs-mode t)
  :custom
  (centaur-tabs-enable-key-bindings t)
  (centaur-tabs-style "bar")
  (centaur-tabs-height 32)
  (centaur-tabs-set-icons t)
  (centaur-tabs-show-new-tab-button t)
  (centaur-tabs-set-close-button t)
  (centaur-tabs-close-button "×")
  (centaur-tabs-show-count t)
  (centaur-tabs-label-fixed-length 15)
  (centaur-tabs-gray-out-icons 'buffer)
  (centaur-tabs-cycle-scope 'tabs)
  :hook
  (dashboard-mode . (lambda () (centaur-tabs-local-mode -1)))
  (term-mode . (lambda () (centaur-tabs-local-mode -1)))
  (eshell-mode . (lambda () (centaur-tabs-local-mode -1)))
  (treemacs-mode . (lambda () (centaur-tabs-local-mode -1)))
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward)
  ("C-S-<iso-lefttab>" . centaur-tabs-move-current-tab-to-left)
  ("C-S-<tab>" . centaur-tabs-move-current-tab-to-right)
  ("C-S-<prior>" . centaur-tabs-move-current-tab-to-left)
  ("C-S-<next>" . centaur-tabs-move-current-tab-to-right)
  ("s-t" . centaur-tabs-new-tab)
  ("s-w" . centaur-tabs-close-current-tab)
  ("s-{" . centaur-tabs-backward)
  ("s-}" . centaur-tabs-forward))

;; Group tabs by project
(defun my/centaur-tabs-buffer-groups ()
  "Group tabs by project or working directory."
  (list (cond
	 ((or (string-equal "*" (substring (buffer-name) 0 1))
	      (memq major-mode '(magit-process-mode
				magit-status-mode
				magit-diff-mode
				magit-log-mode
				magit-stash-mode
				magit-revision-mode
				treemacs-mode)))
	  "Emacs")
	 ((project-current)
	  (project-name (project-current)))
	 (t
	  "Other"))))

(advice-add 'centaur-tabs-buffer-groups :override #'my/centaur-tabs-buffer-groups)

;; ====================
;; Buffers menu configuration
;; ====================
(setq buffers-menu-show-directories 'always)

;; ====================
;; Ibuffer improvements
;; ====================
(use-package ibuffer
  :straight nil
  :bind ("C-x C-b" . ibuffer)
  :hook (ibuffer-mode . ibuffer-auto-mode)
  :custom
  (ibuffer-saved-filter-groups
   '(("default"
      ("Emacs"       (or (name . "^\\*")
			 (mode . help-mode)
			 (mode . completion-list-mode)))
      ("Code"        (or (derived-mode . prog-mode)
			 (derived-mode . text-mode)))
      ("Dired"       (mode . dired-mode))
      ("Treemacs"    (mode . treemacs-mode))
      ("Magit"       (mode . magit-mode))
      ("Term"        (or (mode . term-mode)
			 (mode . eshell-mode)
			 (mode . shell-mode))))))
  (ibuffer-expert t)
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-use-other-window t))

;; Auto-group ibuffer lists
(defun my/ibuffer-set-filter-groups ()
  "Set filter groups for ibuffer."
  (ibuffer-switch-to-saved-filter-groups "default"))
(add-hook 'ibuffer-hook #'my/ibuffer-set-filter-groups)

;; ====================
;; Winner mode - Undo/redo window layout changes
;; ====================
(use-package winner
  :straight nil
  :hook (after-init . winner-mode)
  :bind
  ("s-u" . winner-undo)
  ("s-U" . winner-redo))

(provide 'windows)
;;; windows.el ends here
