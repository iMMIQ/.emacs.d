;; Install and configure the 'which-key' package using use-package with deferred loading
(use-package which-key
  :straight t
  :defer t
  :init
  (setq which-key-show-prefix 'SPC)
  (setq which-key-idle-delay 0.5)
  :config
  (which-key-mode))

;; Install and configure the 'general' package using use-package with deferred loading
(use-package general
  :straight t
  :defer t
  :config
  ;; Declare prefixes after 'general' is loaded
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
      "SPC g" "Version control"
      "SPC g v" "version control"
      "SPC '" "Eshell"
      "SPC SPC" "M-X"))
  
  (eval-after-load 'general
    '(declare-prefixes))

  (general-create-definer emacs-leader
    :prefix "SPC")

  (general-create-definer emacs-local-leader
    :prefix "SPC m"))

(emacs-leader
  :states 'normal
  :keymaps 'override
  ;; Window select
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
  "'" 'eshell
  ;; Others
  "SPC" '(counsel-M-x :wk "M-x")
  )
