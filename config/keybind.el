(straight-use-package 'general)  ;; Install the 'general' package using straight.el
(straight-use-package 'which-key)  ;; Install the 'which-key' package using straight.el

(require 'general)  ;; Load the 'general' package
(require 'which-key)  ;; Load the 'which-key' package

(setq which-key-show-prefix 'SPC)  ;; Set which-key to show key prefixes starting with SPC
(which-key-mode)  ;; Enable which-key mode
(setq which-key-idle-delay 0.5)  ;; Set the idle delay for which-key to 0.5 seconds

(defun declare-prefixes ()
  "Declare prefixes for emacs-leader keybindings."
  (which-key-add-key-based-replacements
    "SPC q" "Quit"
    "SPC f" "Files"
    "SPC b" "Buffers"
    "SPC w" "Windows"
    "SPC e" "Editing"
    "SPC s" "Search"
    "SPC h" "Help"
    "SPC p" "Projectile"
    "SPC g" "Magit"
    "SPC o" "Org Mode"
    "SPC t" "Treemacs"
    "SPC '" "Eshell"
    "SPC !" "Shell command"
    "SPC SPC" "M-X"))

(eval-after-load 'general
  '(declare-prefixes))  ;; Declare prefixes after 'general' is loaded

(general-create-definer emacs-leader
  :prefix "SPC")  ;; Create a general definer for leader key with prefix SPC

(general-create-definer emacs-local-leader
  :prefix "SPC m")  ;; Create a general definer for local leader key with prefix SPC m

(emacs-leader
  :states 'normal
  :keymaps 'override
  ;; Window select
  "1" 'winum-select-window-1
  "2" 'winum-select-window-2
  "3" 'winum-select-window-3
  "4" 'winum-select-window-4
  "5" 'winum-select-window-5
  "6" 'winum-select-window-6
  "7" 'winum-select-window-7
  "8" 'winum-select-window-8
  "9" 'winum-select-window-9
  ;; Quit
  "qq" 'kill-emacs
  "qR" 'restart-emacs
  ;; Files
  "ff" 'find-file
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
  ;; Projectile
  "pf" 'counsel-projectile-find-file
  "pp" 'counsel-projectile-switch-project
  "pb" 'counsel-projectile-switch-to-buffer
  "pa" 'counsel-projectile-ag
  "pg" 'counsel-projectile-rg
  ;; Eshell
  "'" 'eshell
  ;; Others
  "!" 'shell-command
  "SPC" '(counsel-M-x :wk "M-x")
  )
