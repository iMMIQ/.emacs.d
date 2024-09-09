;; Load and configure the 'evil' package for Vim emulation
(use-package evil
  :straight t
  :defer t
  :init
  (evil-mode 1))

;; Load and configure the 'ivy' package for completion and narrowing
(use-package ivy
  :straight t
  :defer t
  :ensure t
  :diminish (ivy-mode)
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  (setq ivy-initial-inputs-alist nil)
  :config
  (ivy-mode 1))

;; Load the 'counsel' package which provides additional commands for 'ivy'
(use-package counsel
  :straight t
  :after ivy
  :config (counsel-mode))

(use-package all-the-icons-ivy-rich
  :straight t
  :defer t
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :straight t
  :defer t
  :ensure t
  :init (ivy-rich-mode 1))

;; Load and configure the 'evil-surround' package for surrounding text objects
(use-package evil-surround
  :straight t
  :defer t
  :config
  (global-evil-surround-mode 1))

;; Load and configure the 'evil-anzu' package for showing search matches
(use-package evil-anzu
  :straight t
  :defer t
  :config
  (global-anzu-mode +1))

;; Load and configure the 'evil-matchit' package for matching text objects
(use-package evil-matchit
  :straight t
  :defer t
  :config
  (global-evil-matchit-mode 1))

;; Load the 'restart-emacs' package for restarting Emacs
(use-package restart-emacs
  :straight t)

;; Load custom keybindings from 'keybind.el'
(load (expand-file-name "config/keybind.el" user-emacs-directory))

;; Set UTF-8 as the default coding system for various environments
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)
(modify-coding-system-alist 'file "" 'utf-8)
(set-language-environment "UTF-8")
(setq buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)
(setq-default default-buffer-file-coding-system 'utf-8)

;; Disable bidirectional text reordering for performance improvement
(setq-default bidi-display-reordering nil)

(pixel-scroll-precision-mode t)
(auto-image-file-mode t)
(global-visual-line-mode t)
(blink-cursor-mode t)
(global-display-line-numbers-mode t)
