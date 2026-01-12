;;; evil.el --- Evil mode configuration

;;; Commentary:
;; Vim emulation configuration using Evil mode and related plugins.

;;; Code:

;; ====================
;; Evil Core
;; ====================

(use-package evil
  :straight t
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-i-jump nil
        evil-respect-visual-line-mode t
        evil-visual-state-cursor 'box
        evil-default-state 'normal)
  :config
  (evil-mode 1))

;; ====================
;; Evil Plugins
;; ====================

(use-package evil-surround
  :straight t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-anzu
  :straight t
  :after evil
  :config
  (global-anzu-mode +1))

(use-package evil-matchit
  :straight t
  :after evil
  :config
  (global-evil-matchit-mode 1))

(provide 'evil-config)
;;; evil.el ends here
