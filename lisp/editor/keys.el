;;; keys.el --- Leader key setup -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'editor-evil "editor/evil")
(require 'editor-windows "editor/windows")
(require 'editor-buffers "editor/buffers")
(require 'editor-editing "editor/editing")
(require 'use-package)

(use-package which-key
  :config
  (which-key-mode 1)
  (which-key-add-key-based-replacements
    "SPC f" "file"
    "SPC b" "buffer"
    "SPC p" "project"
    "SPC g" "git"
    "SPC s" "search"
    "SPC c" "code"
    "SPC w" "window"))

(use-package general
  :after evil
  :config
  (general-override-mode 1)
  (general-create-definer emacs-leader
    :states '(normal visual motion)
    :keymaps 'override
    :prefix "SPC")
  (general-create-definer emacs-local-leader
    :states '(normal visual motion)
    :keymaps 'override
    :prefix "SPC m")
  (general-def
    :states '(normal visual motion)
    :keymaps 'override
    :prefix "SPC"
    "f" '(:ignore t :which-key "file")
    "ff" '(find-file :which-key "find file")
    "fs" '(save-buffer :which-key "save buffer")
    "b" '(:ignore t :which-key "buffer")
    "bb" '(switch-to-buffer :which-key "switch buffer")
    "bd" '(editor-buffers-kill-current :which-key "delete buffer")
    "bn" '(editor-buffers-next :which-key "next buffer")
    "bp" '(editor-buffers-previous :which-key "previous buffer")
    "bl" '(editor-buffers-list :which-key "list buffers")
    "w" '(:ignore t :which-key "window")
    "ws" '(editor-windows-split-below :which-key "split below")
    "wv" '(editor-windows-split-right :which-key "split right")
    "wd" '(editor-windows-delete-window :which-key "delete window")
    "wD" '(editor-windows-delete-other-windows :which-key "delete other windows")
    "c" '(:ignore t :which-key "code")
    "cc" '(editor-editing-comment-line :which-key "comment line")))

(provide 'editor-keys)
;;; keys.el ends here
