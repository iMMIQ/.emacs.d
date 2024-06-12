;; Load and configure the Evil package (Vim emulation in Emacs)
(straight-use-package 'evil)
;; Load and configure the Ivy package (a generic completion mechanism)
(straight-use-package 'ivy)
;; Load and configure the Counsel package (Ivy-enhanced versions of common Emacs commands)
(straight-use-package 'counsel)
;; Load and configure the Evil Surround package (Vim's surround functionality)
(straight-use-package 'evil-surround)
;; Load and configure the Evil Anzu package (enhanced search in Emacs with evil)
(straight-use-package 'evil-anzu)
;; Load and configure the Evil Matchit package (Vim's % matching for tags, etc.)
(straight-use-package 'evil-matchit)

(straight-use-package 'restart-emacs)

;; Enable Evil mode (Vim emulation)
(require 'evil)
(evil-mode 1)

;; Enable Ivy mode (generic completion mechanism)
(require 'ivy)
(ivy-mode 1)

;; Configure Counsel to use virtual buffers
(require 'counsel)
(setq ivy-use-virtual-buffers t)

;; Enable global Evil Surround mode
(require 'evil-surround)
(global-evil-surround-mode 1)

;; Enable global Anzu mode with Evil integration
(require 'evil-anzu)
(global-anzu-mode +1)

;; Enable global Evil Matchit mode
(require 'evil-matchit)
(global-evil-matchit-mode 1)

;; Load additional keybindings from an external file
(load (expand-file-name "config/keybind.el" user-emacs-directory))

;; Set default coding system to UTF-8
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

;; Automatically use UTF-8 encoding when opening files
(modify-coding-system-alist 'file "" 'utf-8)

;; Set language environment to UTF-8
(set-language-environment "UTF-8")

;; Set all new buffers to use UTF-8 encoding by default
(setq buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)
(setq-default default-buffer-file-coding-system 'utf-8)

;; Use UTF-8 encoding when saving files
(add-hook 'before-save-hook 'set-buffer-file-coding-system)

;; Display Unicode characters
(setq-default bidi-display-reordering nil)
