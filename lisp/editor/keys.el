;;; keys.el --- Leader key setup -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'editor-evil "editor/evil")
(require 'editor-windows "editor/windows")
(require 'editor-buffers "editor/buffers")
(require 'editor-editing "editor/editing")
(require 'tools-project "tools/project")
(require 'tools-git "tools/git")
(require 'tools-code "tools/code")
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
    "fr" '(consult-recent-file :which-key "recent files")
    "fs" '(save-buffer :which-key "save buffer")
    "b" '(:ignore t :which-key "buffer")
    "bb" '(consult-buffer :which-key "switch buffer")
    "bd" '(editor-buffers-kill-current :which-key "delete buffer")
    "bn" '(editor-buffers-next :which-key "next buffer")
    "bp" '(editor-buffers-previous :which-key "previous buffer")
    "bl" '(editor-buffers-list :which-key "list buffers")
    "s" '(:ignore t :which-key "search")
    "sg" '(consult-ripgrep :which-key "ripgrep")
    "ss" '(consult-line :which-key "search line")
    "w" '(:ignore t :which-key "window")
    "ws" '(editor-windows-split-below :which-key "split below")
    "wv" '(editor-windows-split-right :which-key "split right")
    "wd" '(editor-windows-delete-window :which-key "delete window")
    "wD" '(editor-windows-delete-other-windows :which-key "delete other windows")
    "c" '(:ignore t :which-key "code")
    "cc" '(editor-editing-comment-line :which-key "comment line"))
  (emacs-leader
    "gs" '(magit-status :which-key "status")
    "ca" '(lsp-bridge-code-action :which-key "code action")
    "cd" '(lsp-bridge-diagnostic-list :which-key "diagnostics")
    "cf" '(apheleia-format :which-key "format")
    "cl" '(lsp-bridge-restart-process :which-key "restart")
    "cn" '(lsp-bridge-diagnostic-jump-next :which-key "next diagnostic")
    "cp" '(lsp-bridge-diagnostic-jump-prev :which-key "previous diagnostic")
    "cr" '(lsp-bridge-rename :which-key "rename")
    "pf" '(my/project-find-file :which-key "find file")
    "pp" '(my/project-switch :which-key "switch project")
    "ps" '(my/project-search :which-key "search project")))

(provide 'editor-keys)
;;; keys.el ends here
