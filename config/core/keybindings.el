;;; keybindings.el --- Global keybindings configuration

;;; Commentary:
;; Centralized keybinding configuration using general.el.

;;; Code:

;; ====================
;; Helper Functions
;; ====================

(defun open-eshell-in-split ()
  "Open eshell in a small window at the bottom."
  (interactive)
  (let ((display-buffer-alist
         '(("\\*eshell\\*"
            (display-buffer-in-side-window)
            (window-height . 0.3)
            (side . bottom)
            (slot . -1)))))
    (eshell)))

;; ====================
;; Global Keybindings
;; ====================

(emacs-leader
  :states 'normal
  :keymaps 'override
  ;; Window select (1-9)
  "1" '(winum-select-window-1 :which-key "select-window-1")
  "2" '(winum-select-window-2 :which-key "select-window-2")
  "3" '(winum-select-window-3 :which-key "select-window-3")
  "4" '(winum-select-window-4 :which-key "select-window-4")
  "5" '(winum-select-window-5 :which-key "select-window-5")
  "6" '(winum-select-window-6 :which-key "select-window-6")
  "7" '(winum-select-window-7 :which-key "select-window-7")
  "8" '(winum-select-window-8 :which-key "select-window-8")
  "9" '(winum-select-window-9 :which-key "select-window-9")
  ;; Quit
  "qq" 'kill-emacs
  "qR" 'restart-emacs
  ;; Files
  "ff" 'counsel-find-file
  "fs" 'save-buffer
  "fr" 'counsel-recentf
  "fR" 'rename-file
  "fd" 'delete-file
  "fD" 'dired
  ;; Buffers
  "bn" 'next-buffer
  "bp" 'previous-buffer
  "bd" 'kill-this-buffer
  "bD" 'kill-buffer
  "bb" 'counsel-switch-buffer
  ;; Window Management
  "wl" 'windmove-right
  "wh" 'windmove-left
  "wk" 'windmove-up
  "wj" 'windmove-down
  "wd" 'delete-window
  "wD" 'delete-other-windows
  "ws" 'split-window-below
  "wv" 'split-window-right
  "w=" 'balance-windows
  ;; Help
  "hK" 'describe-key
  "hf" 'describe-function
  "hv" 'describe-variable
  "hm" 'describe-mode
  ;; Eshell
  "'" 'open-eshell-in-split
  ;; M-x
  "SPC" '(counsel-M-x :wk "M-x")
  ;; Search
  "sg" 'counsel-rg
  "ss" 'swiper
  "sc" 'counsel-git
  "sd" 'counsel-git-grep
  "sp" 'grep-or-fd-ivy
  "sf" 'grep-or-fd)

(provide 'keybindings)
;;; keybindings.el ends here
