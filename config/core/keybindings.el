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
  "qq" '(kill-emacs :which-key "quit-emacs")
  "qR" '(restart-emacs :which-key "restart-emacs")
  ;; Files
  "ff" '(counsel-find-file :which-key "find-file")
  "fs" '(save-buffer :which-key "save-buffer")
  "fr" '(counsel-recentf :which-key "recent-files")
  "fR" '(rename-file :which-key "rename-file")
  "fd" '(delete-file :which-key "delete-file")
  "fD" '(dired :which-key "dired")
  ;; Buffers
  "bn" '(next-buffer :which-key "next-buffer")
  "bp" '(previous-buffer :which-key "previous-buffer")
  "bd" '(kill-this-buffer :which-key "kill-this-buffer")
  "bD" '(kill-buffer :which-key "kill-buffer")
  "bb" '(counsel-switch-buffer :which-key "switch-buffer")
  ;; Window Management
  "wl" '(windmove-right :which-key "window-right")
  "wh" '(windmove-left :which-key "window-left")
  "wk" '(windmove-up :which-key "window-up")
  "wj" '(windmove-down :which-key "window-down")
  "wd" '(delete-window :which-key "delete-window")
  "wD" '(delete-other-windows :which-key "delete-other-windows")
  "ws" '(split-window-below :which-key "split-below")
  "wv" '(split-window-right :which-key "split-right")
  "w=" '(balance-windows :which-key "balance-windows")
  ;; Help
  "hK" '(describe-key :which-key "describe-key")
  "hf" '(describe-function :which-key "describe-function")
  "hv" '(describe-variable :which-key "describe-variable")
  "hm" '(describe-mode :which-key "describe-mode")
  ;; Eshell
  "'" '(open-eshell-in-split :which-key "eshell")
  ;; M-x
  "SPC" '(counsel-M-x :wk "M-x")
  ;; Search
  "sg" '(counsel-rg :which-key "grep-rg")
  "ss" '(swiper :which-key "swiper")
  "sc" '(counsel-git :which-key "counsel-git")
  "sd" '(counsel-git-grep :which-key "git-grep")
  "sp" '(grep-or-fd-ivy :which-key "grep-fd-ivy")
  "sf" '(grep-or-fd :which-key "grep-fd"))

(provide 'keybindings)
;;; keybindings.el ends here
