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
(require 'subr-x)

(autoload 'tools-tree-toggle "tools/tree" nil t)

(defun editor-keys-switch-to-other-buffer ()
  "Switch to the previously current buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) t)))

(defun editor-keys-kill-other-buffers ()
  "Kill other file-visiting buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (when (and (not (eq buffer (current-buffer)))
               (buffer-file-name buffer))
      (kill-buffer buffer))))

(defun editor-keys-kill-buffer-and-window ()
  "Kill the current buffer and close its window when possible."
  (interactive)
  (kill-current-buffer)
  (when (not (one-window-p))
    (delete-window)))

(defun editor-keys-find-files-root ()
  "Find files from the current project root."
  (interactive)
  (if (fboundp 'my/project-find-file)
      (call-interactively #'my/project-find-file)
    (call-interactively #'find-file)))

(defun editor-keys-find-files-cwd ()
  "Find files from `default-directory'."
  (interactive)
  (if (fboundp 'consult-find)
      (consult-find default-directory)
    (call-interactively #'find-file)))

(defun editor-keys-find-config-file ()
  "Find files from `user-emacs-directory'."
  (interactive)
  (if (fboundp 'consult-find)
      (consult-find user-emacs-directory)
    (let ((default-directory user-emacs-directory))
      (call-interactively #'find-file))))

(defun editor-keys-recent-files-cwd ()
  "Open a recent file, matching LazyVim's recent-files entry."
  (interactive)
  (call-interactively #'consult-recent-file))

(defun editor-keys-command-history ()
  "Open command history."
  (interactive)
  (if (fboundp 'consult-complex-command)
      (call-interactively #'consult-complex-command)
    (call-interactively #'execute-extended-command)))

(defun editor-keys-search-word ()
  "Search for the active region or symbol at point in the project."
  (interactive)
  (let ((initial (if (use-region-p)
                     (buffer-substring-no-properties
                      (region-beginning)
                      (region-end))
                   (thing-at-point 'symbol t))))
    (consult-ripgrep nil initial)))

(defun editor-keys-toggle-line-numbers ()
  "Toggle line numbers in the current buffer."
  (interactive)
  (if (bound-and-true-p display-line-numbers-mode)
      (display-line-numbers-mode -1)
    (display-line-numbers-mode 1)))

(defun editor-keys-toggle-wrap ()
  "Toggle visual line wrapping in the current buffer."
  (interactive)
  (visual-line-mode 'toggle))

(defun editor-keys-toggle-spell ()
  "Toggle spell checking in the current buffer."
  (interactive)
  (if (bound-and-true-p flyspell-mode)
      (flyspell-mode -1)
    (flyspell-mode 1)))

(defun editor-keys-redraw ()
  "Redraw the display and clear search highlighting."
  (interactive)
  (when (fboundp 'evil-ex-nohighlight)
    (evil-ex-nohighlight))
  (redraw-display))

(defun editor-keys-open-terminal ()
  "Open an Eshell session."
  (interactive)
  (eshell))

(defun editor-keys-tab-first ()
  "Select the first tab."
  (interactive)
  (tab-bar-select-tab 1))

(defun editor-keys-next-hunk ()
  "Jump to the next Git hunk when diff-hl is available."
  (interactive)
  (require 'diff-hl nil t)
  (if (fboundp 'diff-hl-next-hunk)
      (diff-hl-next-hunk)
    (user-error "diff-hl hunk navigation is unavailable")))

(defun editor-keys-previous-hunk ()
  "Jump to the previous Git hunk when diff-hl is available."
  (interactive)
  (require 'diff-hl nil t)
  (if (fboundp 'diff-hl-previous-hunk)
      (diff-hl-previous-hunk)
    (user-error "diff-hl hunk navigation is unavailable")))

(defun editor-keys-move-line-down ()
  "Move the current line down."
  (interactive)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(defun editor-keys-move-line-up ()
  "Move the current line up."
  (interactive)
  (forward-line -1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(use-package which-key
  :config
  (which-key-mode 1)
  (which-key-add-key-based-replacements
    "SPC <tab>" "tabs"
    "SPC b" "buffer"
    "SPC c" "code"
    "SPC d" "debug"
    "SPC dp" "profiler"
    "SPC f" "file"
    "SPC g" "git"
    "SPC gh" "hunks"
    "SPC q" "quit/session"
    "SPC s" "search"
    "SPC u" "ui"
    "SPC w" "windows"
    "SPC x" "diagnostics/quickfix"))

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
    :states '(normal visual)
    :keymaps 'override
    "C-h" '(windmove-left :which-key "go to left window")
    "C-j" '(windmove-down :which-key "go to lower window")
    "C-k" '(windmove-up :which-key "go to upper window")
    "C-l" '(windmove-right :which-key "go to right window")
    "C-<up>" '(enlarge-window :which-key "increase window height")
    "C-<down>" '(shrink-window :which-key "decrease window height")
    "C-<left>" '(shrink-window-horizontally :which-key "decrease window width")
    "C-<right>" '(enlarge-window-horizontally :which-key "increase window width")
    "M-j" '(editor-keys-move-line-down :which-key "move down")
    "M-k" '(editor-keys-move-line-up :which-key "move up")
    "S-h" '(editor-buffers-previous :which-key "prev buffer")
    "S-l" '(editor-buffers-next :which-key "next buffer")
    "[b" '(editor-buffers-previous :which-key "prev buffer")
    "]b" '(editor-buffers-next :which-key "next buffer")
    "[d" '(lsp-bridge-diagnostic-jump-prev :which-key "prev diagnostic")
    "]d" '(lsp-bridge-diagnostic-jump-next :which-key "next diagnostic")
    "[q" '(previous-error :which-key "previous quickfix")
    "]q" '(next-error :which-key "next quickfix")
    "[h" '(editor-keys-previous-hunk :which-key "prev hunk")
    "]h" '(editor-keys-next-hunk :which-key "next hunk"))
  (general-def
    :states '(normal visual insert)
    :keymaps 'override
    "C-s" '(save-buffer :which-key "save file"))
  (general-def
    :states '(normal visual motion)
    :keymaps 'override
    :prefix "SPC"
    "SPC" '(editor-keys-find-files-root :which-key "find files")
    "," '(consult-buffer :which-key "buffers")
    "/" '(consult-ripgrep :which-key "grep")
    ":" '(editor-keys-command-history :which-key "command history")
    "?" '(describe-keymap :which-key "buffer keymaps")
    "`" '(editor-keys-switch-to-other-buffer :which-key "switch to other buffer")
    "-" '(editor-windows-split-below :which-key "split window below")
    "|" '(editor-windows-split-right :which-key "split window right")
    "<tab>" '(:ignore t :which-key "tabs")
    "<tab>l" '(tab-last :which-key "last tab")
    "<tab>o" '(tab-close-other :which-key "close other tabs")
    "<tab>f" '(editor-keys-tab-first :which-key "first tab")
    "<tab><tab>" '(tab-new :which-key "new tab")
    "<tab>]" '(tab-next :which-key "next tab")
    "<tab>d" '(tab-close :which-key "close tab")
    "<tab>[" '(tab-previous :which-key "previous tab")
    "K" '(evil-lookup :which-key "keywordprg")
    "f" '(:ignore t :which-key "file")
    "fb" '(consult-buffer :which-key "buffers")
    "fB" '(ibuffer :which-key "buffers all")
    "fc" '(editor-keys-find-config-file :which-key "find config file")
    "ff" '(editor-keys-find-files-root :which-key "find files root")
    "fF" '(editor-keys-find-files-cwd :which-key "find files cwd")
    "fg" '(my/project-find-file :which-key "find git files")
    "fn" '(find-file :which-key "new file")
    "fp" '(my/project-switch :which-key "projects")
    "fr" '(consult-recent-file :which-key "recent")
    "fR" '(editor-keys-recent-files-cwd :which-key "recent cwd")
    "fs" '(save-buffer :which-key "save file")
    "ft" '(editor-keys-open-terminal :which-key "terminal root")
    "fT" '(editor-keys-open-terminal :which-key "terminal cwd")
    "b" '(:ignore t :which-key "buffer")
    "bb" '(editor-keys-switch-to-other-buffer :which-key "switch to other buffer")
    "bd" '(editor-buffers-kill-current :which-key "delete buffer")
    "bD" '(editor-keys-kill-buffer-and-window :which-key "delete buffer and window")
    "bn" '(editor-buffers-next :which-key "next buffer")
    "bo" '(editor-keys-kill-other-buffers :which-key "delete other buffers")
    "bp" '(editor-buffers-previous :which-key "previous buffer")
    "bl" '(editor-buffers-list :which-key "list buffers")
    "s" '(:ignore t :which-key "search")
    "s\"" '(consult-register :which-key "registers")
    "s/" '(consult-isearch-history :which-key "search history")
    "sb" '(consult-line :which-key "buffer lines")
    "sB" '(consult-line-multi :which-key "grep open buffers")
    "sc" '(editor-keys-command-history :which-key "command history")
    "sC" '(execute-extended-command :which-key "commands")
    "sd" '(lsp-bridge-diagnostic-list :which-key "diagnostics")
    "sD" '(lsp-bridge-diagnostic-list :which-key "buffer diagnostics")
    "sg" '(consult-ripgrep :which-key "grep root")
    "sG" '(consult-ripgrep :which-key "grep cwd")
    "sh" '(help-for-help :which-key "help pages")
    "sj" '(consult-mark :which-key "jumps")
    "sk" '(describe-key :which-key "keymaps")
    "sl" '(consult-compile-error :which-key "location list")
    "sM" '(consult-man :which-key "man pages")
    "sm" '(consult-mark :which-key "marks")
    "sR" '(consult-history :which-key "resume")
    "sr" '(query-replace :which-key "search and replace")
    "sq" '(consult-compile-error :which-key "quickfix list")
    "ss" '(consult-imenu :which-key "lsp symbols")
    "sS" '(consult-imenu-multi :which-key "lsp workspace symbols")
    "sw" '(editor-keys-search-word :which-key "word root")
    "sW" '(editor-keys-search-word :which-key "word cwd")
    "w" '(:ignore t :which-key "window")
    "wh" '(windmove-left :which-key "go to left window")
    "wj" '(windmove-down :which-key "go to lower window")
    "wk" '(windmove-up :which-key "go to upper window")
    "wl" '(windmove-right :which-key "go to right window")
    "wm" '(delete-other-windows :which-key "maximize window")
    "ws" '(editor-windows-split-below :which-key "split below")
    "wv" '(editor-windows-split-right :which-key "split right")
    "wd" '(editor-windows-delete-window :which-key "delete window")
    "wD" '(editor-windows-delete-other-windows :which-key "delete other windows")
    "q" '(:ignore t :which-key "quit/session")
    "qq" '(save-buffers-kill-emacs :which-key "quit all")
    "u" '(:ignore t :which-key "ui")
    "uC" '(consult-theme :which-key "colorscheme")
    "ud" '(lsp-bridge-diagnostic-list :which-key "diagnostics")
    "ul" '(editor-keys-toggle-line-numbers :which-key "line number")
    "uL" '(editor-keys-toggle-line-numbers :which-key "relative number")
    "ur" '(editor-keys-redraw :which-key "redraw")
    "us" '(editor-keys-toggle-spell :which-key "spelling")
    "uw" '(editor-keys-toggle-wrap :which-key "wrap")
    "x" '(:ignore t :which-key "diagnostics/quickfix")
    "xl" '(consult-compile-error :which-key "location list")
    "xq" '(consult-compile-error :which-key "quickfix list")
    "c" '(:ignore t :which-key "code")
    "cc" '(editor-editing-comment-line :which-key "comment line")
    "cd" '(lsp-bridge-diagnostic-list :which-key "line diagnostics")
    "cf" '(apheleia-format :which-key "format")
    "cF" '(apheleia-format :which-key "format injected langs")
    "cr" '(lsp-bridge-rename :which-key "rename")
    "ca" '(lsp-bridge-code-action :which-key "code action"))
  (general-def
    :states '(normal visual motion)
    :keymaps 'override
    :prefix "SPC"
    "e" '(tools-tree-toggle :which-key "explorer")
    "E" '(tools-tree-toggle :which-key "explorer cwd")
    "fe" '(tools-tree-toggle :which-key "explorer root")
    "fE" '(tools-tree-toggle :which-key "explorer cwd")
    "gg" '(magit-status :which-key "lazygit root")
    "gG" '(magit-status :which-key "lazygit cwd")
    "gL" '(magit-log-all :which-key "git log cwd")
    "gb" '(magit-blame-addition :which-key "git blame line")
    "gd" '(magit-diff-buffer-file :which-key "git diff hunks")
    "gf" '(magit-log-buffer-file :which-key "git current file history")
    "gl" '(magit-log-all :which-key "git log")
    "gs" '(magit-status :which-key "git status")
    "gS" '(magit-stash :which-key "git stash")
    "gh" '(:ignore t :which-key "hunks")
    "ghd" '(magit-diff-buffer-file :which-key "diff this")
    "ghb" '(magit-blame-addition :which-key "blame line")
    "cl" '(lsp-bridge-restart-process :which-key "restart")
    "cn" '(lsp-bridge-diagnostic-jump-next :which-key "next diagnostic")
    "cp" '(lsp-bridge-diagnostic-jump-prev :which-key "previous diagnostic")
    "pf" '(my/project-find-file :which-key "find file")
    "pp" '(my/project-switch :which-key "switch project")
    "ps" '(my/project-search :which-key "search project")))

(provide 'editor-keys)
;;; keys.el ends here
