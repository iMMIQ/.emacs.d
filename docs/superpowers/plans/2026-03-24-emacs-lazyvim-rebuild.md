# Emacs LazyVim-Style Rebuild Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Rebuild this Emacs configuration into a LazyVim-inspired Emacs 30 setup with `evil`, `SPC` leader workflows, a modern minibuffer stack, built-in project support, and retained `lsp-bridge` language integration.

**Architecture:** The rebuild moves startup concerns into `early-init.el`, converts `init.el` into a thin loader, and relocates active configuration into `lisp/{core,editor,ui,tools,lang,personal}`. The old `config/` tree becomes inactive transitional material until the new stack is verified, then it can be removed.

**Tech Stack:** Emacs 30.2, straight.el, use-package, evil, general, which-key, vertico, orderless, marginalia, consult, embark, embark-consult, corfu, cape, project.el, magit, diff-hl, lsp-bridge, apheleia, treemacs, rime, emms

---

## File Structure

### New Files

- `early-init.el`
  - Startup-only performance and UI settings.
- `init.el`
  - Thin bootstrap and top-level module loader.
- `lisp/core/bootstrap.el`
  - straight.el bootstrap and global package defaults.
- `lisp/core/paths.el`
  - load-path setup, cache/custom file paths, shared path helpers.
- `lisp/core/performance.el`
  - startup/runtime tuning that belongs after `early-init.el`.
- `lisp/core/lib.el`
  - shared helper functions used by other modules.
- `lisp/editor/evil.el`
  - evil setup and base Vim behavior.
- `lisp/editor/keys.el`
  - leader, local leader, and global LazyVim-style keymap definitions.
- `lisp/editor/windows.el`
  - window movement, splitting, layout helpers, winner mode, optional winum.
- `lisp/editor/buffers.el`
  - buffer switching, cleanup, ibuffer, recentf, and related commands.
- `lisp/editor/editing.el`
  - editing helpers such as smartparens and comment helpers.
- `lisp/ui/theme.el`
  - theme selection and explicit theme loading.
- `lisp/ui/modeline.el`
  - modeline package setup.
- `lisp/ui/startup.el`
  - dashboard and startup screen behavior.
- `lisp/ui/display.el`
  - line numbers, fringe, menu/tool/scroll bars, popup policy.
- `lisp/tools/completion.el`
  - vertico/orderless/marginalia stack.
- `lisp/tools/actions.el`
  - embark and embark-consult setup.
- `lisp/tools/search.el`
  - consult-based search commands and wrappers.
- `lisp/tools/project.el`
  - built-in project.el helpers and project commands.
- `lisp/tools/git.el`
  - magit and diff-hl integration.
- `lisp/tools/shell.el`
  - eshell and terminal helpers.
- `lisp/tools/tree.el`
  - optional treemacs integration.
- `lisp/tools/code.el`
  - shared `SPC c` bindings for formatting, diagnostics, symbols, and `lsp-bridge`.
- `lisp/lang/base.el`
  - shared language defaults.
- `lisp/lang/python.el`
  - Python mode, formatters, `lsp-bridge`, and local extras.
- `lisp/lang/rust.el`
  - Rust mode, formatters, `lsp-bridge`, and local extras.
- `lisp/lang/verilog.el`
  - Verilog mode, formatter, `lsp-bridge`.
- `lisp/personal/chinese.el`
  - input method and Chinese text tooling.
- `lisp/personal/music.el`
  - EMMS setup.
- `test/config-load.el`
  - ERT smoke tests for loading core modules in batch mode.

### Transitional or Removed Files

- `config/`
  - Leave in place during migration; deactivate from `init.el`.
- `README.md`
  - Update architecture and usage notes after rebuild is working.

## Verification Commands

- Batch smoke test:
  - `emacs --batch -Q -l init.el -l test/config-load.el -f ert-run-tests-batch-and-exit`
- Plain startup smoke test:
  - `emacs --batch -Q -l init.el --eval '(princ "ok\n")'`
- Manual interactive validation:
  - `emacs -Q -l init.el`

## Task 1: Establish Startup Skeleton

**Files:**
- Create: `early-init.el`
- Modify: `init.el`
- Create: `lisp/core/bootstrap.el`
- Create: `lisp/core/paths.el`
- Create: `lisp/core/performance.el`
- Create: `lisp/core/lib.el`
- Test: `test/config-load.el`

- [ ] **Step 1: Write the failing startup smoke test**

```elisp
(require 'ert)

(ert-deftest config-smoke/init-loads ()
  (should (featurep 'init)))
```

- [ ] **Step 2: Run test to verify it fails**

Run: `emacs --batch -Q -l init.el -l test/config-load.el -f ert-run-tests-batch-and-exit`
Expected: FAIL because `test/config-load.el` does not exist yet or because the new loader has not been wired.

- [ ] **Step 3: Create startup files with minimal loader behavior**

```elisp
;; early-init.el
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      package-enable-at-startup nil)
```

```elisp
;; init.el
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'core-paths)
(require 'core-bootstrap)
(require 'core-performance)
(provide 'init)
```

- [ ] **Step 4: Expand loader to require all top-level module groups**

```elisp
(dolist (feature '(editor-evil
                   editor-keys
                   ui-theme
                   tools-completion))
  (require feature))
```

- [ ] **Step 5: Run smoke test to verify it passes**

Run: `emacs --batch -Q -l init.el -l test/config-load.el -f ert-run-tests-batch-and-exit`
Expected: PASS for `config-smoke/init-loads`

- [ ] **Step 6: Commit**

```bash
git add early-init.el init.el lisp/core test/config-load.el
git commit -m "refactor: create startup skeleton"
```

## Task 2: Rebuild the Editor Core

**Files:**
- Create: `lisp/editor/evil.el`
- Create: `lisp/editor/keys.el`
- Create: `lisp/editor/windows.el`
- Create: `lisp/editor/buffers.el`
- Create: `lisp/editor/editing.el`
- Test: `test/config-load.el`

- [ ] **Step 1: Write a failing test for leader infrastructure**

```elisp
(ert-deftest config-smoke/leader-functions-exist ()
  (should (fboundp 'emacs-leader))
  (should (fboundp 'emacs-local-leader)))
```

- [ ] **Step 2: Run test to verify it fails**

Run: `emacs --batch -Q -l init.el -l test/config-load.el -f ert-run-tests-batch-and-exit`
Expected: FAIL because the new key infrastructure is not implemented yet.

- [ ] **Step 3: Implement minimal evil and leader setup**

```elisp
(use-package evil
  :straight t
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package general
  :straight t
  :config
  (general-create-definer emacs-leader :states '(normal visual motion) :prefix "SPC")
  (general-create-definer emacs-local-leader :states '(normal visual motion) :prefix "SPC m"))
```

- [ ] **Step 4: Add LazyVim-style groups and core window and buffer helpers**

```elisp
(which-key-add-key-based-replacements
  "SPC f" "file"
  "SPC b" "buffer"
  "SPC p" "project"
  "SPC g" "git"
  "SPC s" "search"
  "SPC c" "code")
```

- [ ] **Step 5: Run smoke test to verify it passes**

Run: `emacs --batch -Q -l init.el -l test/config-load.el -f ert-run-tests-batch-and-exit`
Expected: PASS including `config-smoke/leader-functions-exist`

- [ ] **Step 6: Commit**

```bash
git add lisp/editor test/config-load.el
git commit -m "refactor: rebuild evil and leader core"
```

## Task 3: Replace Completion and Search Stack

**Files:**
- Create: `lisp/tools/completion.el`
- Create: `lisp/tools/actions.el`
- Create: `lisp/tools/search.el`
- Modify: `lisp/editor/keys.el`
- Test: `test/config-load.el`

- [ ] **Step 1: Write failing tests for the new interactive stack**

```elisp
(ert-deftest config-smoke/completion-commands-exist ()
  (should (fboundp 'consult-find))
  (should (fboundp 'consult-buffer))
  (should (fboundp 'embark-act)))
```

- [ ] **Step 2: Run test to verify it fails**

Run: `emacs --batch -Q -l init.el -l test/config-load.el -f ert-run-tests-batch-and-exit`
Expected: FAIL because `consult` and `embark` are not loaded yet.

- [ ] **Step 3: Implement vertico, orderless, marginalia, consult, and embark**

```elisp
(use-package vertico
  :straight t
  :init
  (vertico-mode 1))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic)))

(use-package consult :straight t)
(use-package embark :straight t)
```

- [ ] **Step 4: Rebind file, buffer, and search commands to the new stack**

```elisp
(emacs-leader
  "ff" #'find-file
  "fr" #'consult-recent-file
  "bb" #'consult-buffer
  "sg" #'consult-ripgrep
  "ss" #'consult-line)
```

- [ ] **Step 5: Run smoke test to verify it passes**

Run: `emacs --batch -Q -l init.el -l test/config-load.el -f ert-run-tests-batch-and-exit`
Expected: PASS including `config-smoke/completion-commands-exist`

- [ ] **Step 6: Commit**

```bash
git add lisp/tools/completion.el lisp/tools/actions.el lisp/tools/search.el lisp/editor/keys.el test/config-load.el
git commit -m "refactor: replace ivy stack with consult stack"
```

## Task 4: Replace Projectile with Native Project Support

**Files:**
- Create: `lisp/tools/project.el`
- Modify: `lisp/editor/keys.el`
- Test: `test/config-load.el`

- [ ] **Step 1: Write a failing test for project helpers**

```elisp
(ert-deftest config-smoke/project-helpers-exist ()
  (should (fboundp 'my/project-find-file))
  (should (fboundp 'my/project-switch)))
```

- [ ] **Step 2: Run test to verify it fails**

Run: `emacs --batch -Q -l init.el -l test/config-load.el -f ert-run-tests-batch-and-exit`
Expected: FAIL because the helper commands are not defined yet.

- [ ] **Step 3: Implement thin wrappers around built-in `project.el`**

```elisp
(defun my/project-find-file ()
  (interactive)
  (call-interactively #'project-find-file))

(defun my/project-switch ()
  (interactive)
  (call-interactively #'project-switch-project))
```

- [ ] **Step 4: Bind project commands under `SPC p`**

```elisp
(emacs-leader
  "pf" #'my/project-find-file
  "pp" #'my/project-switch
  "ps" #'consult-ripgrep)
```

- [ ] **Step 5: Run smoke test to verify it passes**

Run: `emacs --batch -Q -l init.el -l test/config-load.el -f ert-run-tests-batch-and-exit`
Expected: PASS including `config-smoke/project-helpers-exist`

- [ ] **Step 6: Commit**

```bash
git add lisp/tools/project.el lisp/editor/keys.el test/config-load.el
git commit -m "refactor: move to built-in project support"
```

## Task 5: Rebuild Git and Code Actions

**Files:**
- Create: `lisp/tools/git.el`
- Create: `lisp/tools/code.el`
- Modify: `lisp/editor/keys.el`
- Test: `test/config-load.el`

- [ ] **Step 1: Write failing tests for git and code commands**

```elisp
(ert-deftest config-smoke/git-and-code-bindings-exist ()
  (should (fboundp 'magit-status))
  (should (fboundp 'apheleia-format))
  (should (fboundp 'lsp-bridge-code-action)))
```

- [ ] **Step 2: Run test to verify it fails**

Run: `emacs --batch -Q -l init.el -l test/config-load.el -f ert-run-tests-batch-and-exit`
Expected: FAIL until the new tool modules are loaded.

- [ ] **Step 3: Implement magit and diff-hl integration**

```elisp
(use-package magit :straight t)
(use-package diff-hl
  :straight t
  :config
  (global-diff-hl-mode 1))
```

- [ ] **Step 4: Implement shared `SPC c` mappings**

```elisp
(emacs-leader
  "ca" #'lsp-bridge-code-action
  "cr" #'lsp-bridge-rename
  "cf" #'apheleia-format
  "cd" #'lsp-bridge-diagnostic-list
  "cn" #'lsp-bridge-diagnostic-jump-next
  "cp" #'lsp-bridge-diagnostic-jump-prev
  "cl" #'lsp-bridge-restart-process)
```

- [ ] **Step 5: Run smoke test to verify it passes**

Run: `emacs --batch -Q -l init.el -l test/config-load.el -f ert-run-tests-batch-and-exit`
Expected: PASS including `config-smoke/git-and-code-bindings-exist`

- [ ] **Step 6: Commit**

```bash
git add lisp/tools/git.el lisp/tools/code.el lisp/editor/keys.el test/config-load.el
git commit -m "refactor: rebuild git and code action modules"
```

## Task 6: Rebuild UI and Optional Tree Tooling

**Files:**
- Create: `lisp/ui/theme.el`
- Create: `lisp/ui/modeline.el`
- Create: `lisp/ui/startup.el`
- Create: `lisp/ui/display.el`
- Create: `lisp/tools/tree.el`
- Test: `test/config-load.el`

- [ ] **Step 1: Write a failing smoke test for display modules**

```elisp
(ert-deftest config-smoke/display-features-load ()
  (should (featurep 'ui-display))
  (should (featurep 'ui-theme)))
```

- [ ] **Step 2: Run test to verify it fails**

Run: `emacs --batch -Q -l init.el -l test/config-load.el -f ert-run-tests-batch-and-exit`
Expected: FAIL because the new UI modules do not exist yet.

- [ ] **Step 3: Implement explicit, side-effect free UI modules**

```elisp
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
```

- [ ] **Step 4: Add optional `treemacs` integration without making it core**

```elisp
(use-package treemacs
  :straight t
  :commands (treemacs treemacs-select-window))
```

- [ ] **Step 5: Run smoke test to verify it passes**

Run: `emacs --batch -Q -l init.el -l test/config-load.el -f ert-run-tests-batch-and-exit`
Expected: PASS including `config-smoke/display-features-load`

- [ ] **Step 6: Commit**

```bash
git add lisp/ui lisp/tools/tree.el test/config-load.el
git commit -m "refactor: rebuild UI modules"
```

## Task 7: Rebuild Language Modules Around `lsp-bridge`

**Files:**
- Create: `lisp/lang/base.el`
- Create: `lisp/lang/python.el`
- Create: `lisp/lang/rust.el`
- Create: `lisp/lang/verilog.el`
- Modify: `lisp/tools/code.el`
- Test: `test/config-load.el`

- [ ] **Step 1: Write failing tests for language module presence**

```elisp
(ert-deftest config-smoke/language-features-load ()
  (should (featurep 'lang-python))
  (should (featurep 'lang-rust))
  (should (featurep 'lang-verilog)))
```

- [ ] **Step 2: Run test to verify it fails**

Run: `emacs --batch -Q -l init.el -l test/config-load.el -f ert-run-tests-batch-and-exit`
Expected: FAIL because the new language modules are not created yet.

- [ ] **Step 3: Implement shared `lsp-bridge` and formatter plumbing**

```elisp
(use-package lsp-bridge
  :straight (lsp-bridge :host github
                        :repo "manateelazycat/lsp-bridge"
                        :files ("*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")))

(use-package apheleia
  :straight t
  :config
  (apheleia-global-mode 1))
```

- [ ] **Step 4: Move Python, Rust, and Verilog specifics into separate modules**

```elisp
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :hook (python-mode . lsp-bridge-mode))
```

```elisp
(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :hook (rustic-mode . lsp-bridge-mode))
```

- [ ] **Step 5: Run smoke test to verify it passes**

Run: `emacs --batch -Q -l init.el -l test/config-load.el -f ert-run-tests-batch-and-exit`
Expected: PASS including `config-smoke/language-features-load`

- [ ] **Step 6: Commit**

```bash
git add lisp/lang lisp/tools/code.el test/config-load.el
git commit -m "refactor: rebuild language modules"
```

## Task 8: Reconnect Personal Modules Safely

**Files:**
- Create: `lisp/personal/chinese.el`
- Create: `lisp/personal/music.el`
- Modify: `init.el`
- Test: `test/config-load.el`

- [ ] **Step 1: Write a failing test for optional personal modules**

```elisp
(ert-deftest config-smoke/personal-modules-do-not-break-load ()
  (should (featurep 'init)))
```

- [ ] **Step 2: Run test to verify it fails for the right reason**

Run: `emacs --batch -Q -l init.el -l test/config-load.el -f ert-run-tests-batch-and-exit`
Expected: FAIL only if the personal module wiring breaks the loader.

- [ ] **Step 3: Rebuild personal modules with optional dependency guards**

```elisp
(use-package rime
  :straight t
  :defer t)

(use-package emms
  :straight t
  :defer t)
```

- [ ] **Step 4: Load personal modules late and defensively**

```elisp
(condition-case err
    (require 'personal-chinese)
  (error (message "Skipping chinese module: %s" err)))
```

- [ ] **Step 5: Run smoke test to verify it passes**

Run: `emacs --batch -Q -l init.el -l test/config-load.el -f ert-run-tests-batch-and-exit`
Expected: PASS with personal modules either loaded or skipped cleanly.

- [ ] **Step 6: Commit**

```bash
git add lisp/personal init.el test/config-load.el
git commit -m "refactor: isolate personal modules"
```

## Task 9: Remove Old Active Configuration and Update Docs

**Files:**
- Modify: `README.md`
- Delete or leave inactive: `config/`
- Test: `test/config-load.el`

- [ ] **Step 1: Write a failing documentation or cleanup check**

```elisp
(ert-deftest config-smoke/new-layout-is-active ()
  (should (featurep 'init)))
```

- [ ] **Step 2: Run test to verify it covers the active layout**

Run: `emacs --batch -Q -l init.el -l test/config-load.el -f ert-run-tests-batch-and-exit`
Expected: PASS or FAIL only based on the new loader, not the legacy tree.

- [ ] **Step 3: Remove the old `config/` tree from the active path**

```elisp
;; init.el should no longer load files from config/
```

- [ ] **Step 4: Update README for the new architecture and key workflow**

```markdown
## Structure

- `early-init.el`
- `init.el`
- `lisp/`
```

- [ ] **Step 5: Run full smoke verification**

Run: `emacs --batch -Q -l init.el -l test/config-load.el -f ert-run-tests-batch-and-exit`
Expected: PASS

Run: `emacs --batch -Q -l init.el --eval '(princ "ok\n")'`
Expected: prints `ok`

- [ ] **Step 6: Commit**

```bash
git add README.md init.el lisp test/config-load.el
git commit -m "docs: update rebuilt config layout"
```
