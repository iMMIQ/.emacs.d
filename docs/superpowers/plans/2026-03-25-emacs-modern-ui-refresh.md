# Emacs Modern UI Refresh Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Modernize the active Emacs UI shell with `doom-one`, improved typography and spacing, a restrained `doom-modeline`, a minimal `dashboard`, and light `treemacs` alignment while preserving resilient startup behavior.

**Architecture:** Keep all active UI work inside `lisp/ui/` plus light optional tree integration in `lisp/tools/tree.el`. `lisp/core/bootstrap.el` continues to call `ui-theme-apply`; `ui-theme-apply` centrally prepares already-built optional UI packages, then delegates to the individual UI modules for display, modeline, startup, and theme behavior. Missing optional packages must always degrade cleanly.

**Tech Stack:** Emacs Lisp, straight.el, use-package, doom-themes, doom-modeline, dashboard, treemacs, ERT

---

## File Structure

### Modified Files

- `lisp/ui/theme.el`
  - Bootstrap optional UI packages early enough for the top-level UI load path.
  - Set `doom-one` as the default theme target.
  - Guard theme loading so startup succeeds when `doom-themes` is unavailable.
- `lisp/ui/display.el`
  - Apply frame/UI shell defaults, GUI-only font and spacing changes, line-number scope, and split preferences.
- `lisp/ui/modeline.el`
  - Configure built-in modeline fallback plus optional `doom-modeline`.
- `lisp/ui/startup.el`
  - Keep startup suppression and add minimal `dashboard` integration when available.
- `lisp/tools/tree.el`
  - Add lazy `treemacs` entrypoints and light visual alignment without making tree tooling mandatory.
- `lisp/editor/keys.el`
  - Add a lazy keybinding or command path that can invoke tree tooling without loading it during startup.
- `test/config-load.el`
  - Extend smoke coverage for theme defaults, display defaults, and startup resilience when optional UI packages are absent.

### Unchanged Integration Points

- `lisp/core/bootstrap.el`
  - Continues to invoke `ui-theme-apply` as the concrete UI entrypoint.
- `init.el`
  - Remains the thin startup entry point.

## Verification Commands

- Full smoke suite:
  - `emacs --batch -Q -l init.el -l test/config-load.el -f ert-run-tests-batch-and-exit`
- Plain batch init smoke:
  - `emacs --batch -Q -l init.el --eval '(princ "ok\n")'`
- Manual GUI validation:
  - `emacs -Q -l init.el`

## Task 1: Add UI Theme and Display Smoke Coverage

**Files:**
- Modify: `test/config-load.el`

- [ ] **Step 1: Write a failing test for the theme default**

```elisp
(ert-deftest config-smoke/ui-theme-default-is-doom-one ()
  (config-smoke--ensure-init-loaded)
  (should (eq ui-theme-default 'doom-one)))
```

- [ ] **Step 2: Write a failing test for scoped display defaults**

```elisp
(ert-deftest config-smoke/display-defaults-are-modern-but-scoped ()
  (config-smoke--ensure-init-loaded)
  (should (equal frame-title-format nil))
  (should (< (abs (- line-spacing 0.16)) 0.0001))
  (should (memq #'display-line-numbers-mode prog-mode-hook))
  (should-not (bound-and-true-p global-display-line-numbers-mode)))
```

- [ ] **Step 3: Write a failing subprocess test for missing optional UI packages**

```elisp
(ert-deftest config-smoke/init-loads-when-optional-ui-packages-are-missing ()
  (let ((result (config-smoke--init-load-result-with-ui-package-failures)))
    (should (equal (plist-get result :status) 0))
    (pcase-let ((`(:init ,init :theme ,theme)
                 (plist-get result :data)))
      (should init)
      (should theme))))
```

- [ ] **Step 4: Add the helper used by the subprocess test**

```elisp
(defun config-smoke--init-load-result-with-ui-package-failures ()
  (let* ((default-directory config-smoke--root-dir)
         (init-file (expand-file-name "init.el" config-smoke--root-dir))
         (output-buffer (generate-new-buffer " *config-smoke-ui-fallback*"))
         (form
          `(let ((user-emacs-directory ,config-smoke--root-dir)
                 (load-prefer-newer t))
             (require 'cl-lib)
             (let ((real-require (symbol-function 'require)))
               (cl-letf (((symbol-function 'require)
                          (lambda (feature &optional filename noerror)
                            (if (memq feature '(doom-themes doom-modeline dashboard nerd-icons))
                                (if noerror nil (signal 'error (list feature)))
                              (funcall real-require feature filename noerror)))))
                 (load ,init-file nil 'nomessage)))
             (princ "RESULT ")
             (princ
              (prin1-to-string
               (list :init (featurep 'init)
                     :theme (featurep 'ui-theme)))))))
    (unwind-protect
        (let ((status (call-process "emacs" nil output-buffer nil "--batch" "-Q"
                                    "--eval" (prin1-to-string form))))
          (with-current-buffer output-buffer
            (let ((output (buffer-string))
                  (result-start nil))
              (setq result-start (string-match "RESULT " output))
              (list :status status
                    :data (and result-start
                               (read (substring output
                                                (+ result-start
                                                   (length "RESULT ")))))))))
      (kill-buffer output-buffer))))
```

- [ ] **Step 5: Run the UI smoke tests to verify they fail for the right reasons**

Run: `emacs --batch -Q -l init.el -l test/config-load.el -f ert-run-tests-batch-and-exit`
Expected: FAIL because `ui-theme-default` is still `modus-operandi`, display defaults are not yet modernized, and missing-package fallback handling is not implemented.

- [ ] **Step 6: Commit**

```bash
git add test/config-load.el
git commit -m "test: add ui shell smoke coverage"
```

## Task 2: Implement `doom-one` Theme and Display Shell Defaults

**Files:**
- Modify: `lisp/ui/theme.el`
- Modify: `lisp/ui/display.el`
- Modify: `test/config-load.el`

- [ ] **Step 1: Make already-built optional UI packages visible without hard depending on `use-package`**

```elisp
(defconst ui-theme--straight-build-root
  (expand-file-name "straight/build" user-emacs-directory))

(defun ui-theme--add-package-to-load-path (package)
  (let ((dir (expand-file-name package ui-theme--straight-build-root)))
    (when (file-directory-p dir)
      (add-to-list 'load-path dir))))

(defun ui-theme--prepare-optional-packages ()
  (dolist (package '("doom-themes" "doom-modeline" "dashboard" "nerd-icons"))
    (ui-theme--add-package-to-load-path package)))

(defun ui-icon-capable-p ()
  (and (display-graphic-p)
       (require 'nerd-icons nil t)
       (or (member "Symbols Nerd Font Mono" (font-family-list))
           (member "Symbols Nerd Font" (font-family-list)))))
```

- [ ] **Step 2: Set `doom-one` as the default and load it safely**

```elisp
(defconst ui-theme-default 'doom-one
  "Default theme loaded by the UI shell.")

(defun ui-theme--load-default-theme ()
  (mapc #'disable-theme (copy-sequence custom-enabled-themes))
  (if (require 'doom-themes nil t)
      (load-theme ui-theme-default t)
    (ignore-errors (load-theme 'deeper-blue t))))

(defun ui-theme-apply ()
  (interactive)
  (ui-theme--prepare-optional-packages)
  (ui-startup-apply)
  (ui-display-apply)
  (ui-modeline-apply)
  (ui-theme--load-default-theme))
```

- [ ] **Step 3: Implement GUI-safe display polish in `lisp/ui/display.el`**

```elisp
(defconst ui-display-font-preferences
  '("JetBrains Mono" "Iosevka Comfy" "Sarasa Mono SC"))

(defun ui-display--set-first-available-font ()
  (when (display-graphic-p)
    (let ((font (seq-find (lambda (family)
                            (member family (font-family-list)))
                          ui-display-font-preferences)))
      (when font
        (set-face-attribute 'default nil :font font :height 140)))))

(defun ui-display-apply ()
  (setq frame-title-format nil
        split-width-threshold 160
        split-height-threshold nil)
  (setq-default line-spacing 0.16)
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (when (display-graphic-p)
    (setq-default internal-border-width 12)
    (add-to-list 'default-frame-alist '(internal-border-width . 12)))
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
  (ui-display--set-first-available-font)
  (when (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1)))
```

- [ ] **Step 4: Tighten the helper implementation and tests until the new smoke tests pass**

```elisp
;; Assert only behavior that matters to the spec:
;; - `ui-theme-default` is `doom-one`
;; - `frame-title-format` is nil
;; - `line-spacing` is `0.16`
;; - GUI frames get subtle padding through `internal-border-width`
;; - line numbers are enabled through `prog-mode-hook`, not globally
;; - init survives missing `doom-themes`, `doom-modeline`, `dashboard`, and `nerd-icons`
;; - existing smoke tests that currently pin `modus-operandi` are updated to
;;   expect `doom-one`, including `config-smoke/ui-theme-apply-is-idempotent`
;;   and `config-smoke/init-applies-ui-through-bootstrap`
```

- [ ] **Step 5: Run the full smoke suite**

Run: `emacs --batch -Q -l init.el -l test/config-load.el -f ert-run-tests-batch-and-exit`
Expected: PASS for `config-smoke/ui-theme-default-is-doom-one`, `config-smoke/display-defaults-are-modern-but-scoped`, and `config-smoke/init-loads-when-optional-ui-packages-are-missing`.

- [ ] **Step 6: Commit**

```bash
git add lisp/ui/theme.el lisp/ui/display.el test/config-load.el
git commit -m "feat: modernize theme and display shell"
```

## Task 3: Add Modeline and Startup Dashboard Tests

**Files:**
- Modify: `test/config-load.el`

- [ ] **Step 1: Write a failing test for modeline fallback behavior**

```elisp
(ert-deftest config-smoke/modeline-setup-stays-safe-without-doom-modeline ()
  (let ((result (config-smoke--ui-module-load-result
                 "lisp/ui/modeline.el"
                 'ui-modeline
                 '(doom-modeline nerd-icons))))
    (should (equal (plist-get result :status) 0))
    (should (plist-get result :feature))))
```

- [ ] **Step 2: Write a failing test for startup dashboard fallback behavior**

```elisp
(ert-deftest config-smoke/startup-setup-stays-safe-without-dashboard ()
  (let ((result (config-smoke--ui-module-load-result
                 "lisp/ui/startup.el"
                 'ui-startup
                 '(dashboard nerd-icons))))
    (should (equal (plist-get result :status) 0))
    (should (plist-get result :feature))))
```

- [ ] **Step 3: Add a reusable helper for isolated UI module loading**

```elisp
(defun config-smoke--ui-module-load-result (module-path expected-feature missing-features)
  (let* ((default-directory config-smoke--root-dir)
         (lisp-dir (expand-file-name "lisp" config-smoke--root-dir))
         (full-path (expand-file-name module-path config-smoke--root-dir))
         (output-buffer (generate-new-buffer " *config-smoke-ui-module*"))
         (form
          `(let ((load-prefer-newer t))
             (require 'cl-lib)
             (defmacro use-package (_name &rest _args) nil)
             (provide 'use-package)
             (add-to-list 'load-path ,lisp-dir)
             (let ((real-require (symbol-function 'require)))
               (cl-letf (((symbol-function 'require)
                          (lambda (feature &optional filename noerror)
                            (if (memq feature ',missing-features)
                                (if noerror nil (signal 'error (list feature)))
                              (funcall real-require feature filename noerror)))))
                 (load ,full-path nil 'nomessage)))
             (princ "RESULT ")
             (princ
              (prin1-to-string
               (list :feature (featurep ',expected-feature)))))))
    (unwind-protect
        (let ((status (call-process "emacs" nil output-buffer nil "--batch" "-Q"
                                    "--eval" (prin1-to-string form))))
          (with-current-buffer output-buffer
            (let ((output (buffer-string))
                  (result-start nil))
              (setq result-start (string-match "RESULT " output))
              (list :status status
                    :feature (and result-start
                                  (plist-get
                                   (read (substring output
                                                    (+ result-start
                                                       (length "RESULT "))))
                                   :feature))))))
      (kill-buffer output-buffer))))
```

- [ ] **Step 4: Run the suite to verify the new tests fail before implementation**

Run: `emacs --batch -Q -l init.el -l test/config-load.el -f ert-run-tests-batch-and-exit`
Expected: FAIL because `lisp/ui/modeline.el` and `lisp/ui/startup.el` do not yet guard optional package setup.

- [ ] **Step 5: Commit**

```bash
git add test/config-load.el
git commit -m "test: add startup and modeline fallback coverage"
```

## Task 4: Implement Modeline and Startup Modules

**Files:**
- Modify: `lisp/ui/modeline.el`
- Modify: `lisp/ui/startup.el`
- Modify: `test/config-load.el`

- [ ] **Step 1: Add conservative `doom-modeline` setup with built-in fallback**

```elisp
(defun ui-modeline-apply ()
  (line-number-mode 1)
  (column-number-mode 1)
  (size-indication-mode 1)
  (when (require 'doom-modeline nil t)
    (setq doom-modeline-height 24
          doom-modeline-bar-width 3
          doom-modeline-buffer-file-name-style 'truncate-upto-project
          doom-modeline-minor-modes nil
          doom-modeline-buffer-encoding nil
          doom-modeline-indent-info nil
          doom-modeline-icon (ui-icon-capable-p))
    (doom-modeline-mode 1)))
```

- [ ] **Step 2: Add restrained `dashboard` startup integration with fallback**

```elisp
(defvar ui-startup--dashboard-hook-installed nil)

(defun ui-startup-apply ()
  (setq inhibit-startup-screen t)
  (when (require 'dashboard nil t)
    (setq dashboard-banner-logo-title "Emacs"
          dashboard-startup-banner 'official
          dashboard-center-content t
          dashboard-show-shortcuts nil
          dashboard-footer-messages nil
          dashboard-set-heading-icons (ui-icon-capable-p)
          dashboard-set-file-icons (ui-icon-capable-p)
          dashboard-items '((recents . 6)
                            (projects . 5)
                            (bookmarks . 4)))
    (unless ui-startup--dashboard-hook-installed
      (dashboard-setup-startup-hook)
      (setq ui-startup--dashboard-hook-installed t))))
```

- [ ] **Step 3: Keep icon behavior optional**

```elisp
;; Enable icon-dependent UI only when both the package and a Nerd Font family
;; are available. Otherwise force plain-text fallbacks:
(unless (ui-icon-capable-p)
  (setq doom-modeline-icon nil
        dashboard-set-heading-icons nil
        dashboard-set-file-icons nil))
```

- [ ] **Step 4: Run the full smoke suite**

Run: `emacs --batch -Q -l init.el -l test/config-load.el -f ert-run-tests-batch-and-exit`
Expected: PASS including the new modeline and startup fallback tests.

- [ ] **Step 5: Manually verify the GUI shell**

Run: `emacs -Q -l init.el`
Expected: GUI Emacs opens with `doom-one`, a restrained modern modeline, and a minimal dashboard without startup errors.

- [ ] **Step 6: Commit**

```bash
git add lisp/ui/modeline.el lisp/ui/startup.el test/config-load.el
git commit -m "feat: add modern modeline and startup ui"
```

## Task 5: Align `treemacs` and Finish Verification

**Files:**
- Modify: `lisp/tools/tree.el`
- Modify: `lisp/editor/keys.el`
- Modify: `test/config-load.el`

- [ ] **Step 1: Add a smoke test that tree tooling stays out of the startup path**

```elisp
(ert-deftest config-smoke/tree-module-stays-optional-at-startup ()
  (config-smoke--ensure-init-loaded)
  (should-not (featurep 'tools-tree))
  (should-not (memq 'tools-tree core-bootstrap-top-level-features)))
```

- [ ] **Step 2: Add a focused lazy-load test for the optional tree path**

```elisp
(ert-deftest config-smoke/tree-entrypoint-loads-module-on-demand ()
  (let ((result (config-smoke--tree-entrypoint-load-result)))
    (should (equal (plist-get result :status) 0))
    (should (plist-get result :feature))
    (should (plist-get result :entrypoint))))

(ert-deftest config-smoke/tree-entrypoint-is-bound-after-init ()
  (config-smoke--ensure-init-loaded)
  (should (eq (config-smoke--leader-binding 'normal "SPC e")
              'tools-tree-toggle)))
```

- [ ] **Step 3: Add a lazy tree entrypoint in `lisp/editor/keys.el` and implement aligned tree setup**

```elisp
;; In lisp/tools/tree.el
(defun tools-tree-toggle ()
  (interactive)
  (require 'tools-tree "tools/tree")
  (treemacs))

(use-package treemacs
  :straight t
  :commands (treemacs treemacs-select-window)
  :config
  (setq treemacs-width 32)
  (treemacs-follow-mode 1)
  (treemacs-filewatch-mode 1)
  (when (require 'doom-themes nil t)
    (when (fboundp 'doom-themes-treemacs-config)
      (doom-themes-treemacs-config))))

;; In lisp/editor/keys.el
(autoload 'tools-tree-toggle "tools/tree" nil t)
(emacs-leader
  "e" '(tools-tree-toggle :which-key "explorer"))
```

- [ ] **Step 4: Keep nonessential plugins out of the critical path**

```elisp
;; Do not add `tools-tree` to the top-level startup chain.
;; Do not require treemacs-all-the-icons or similar extras.
;; Any package fetches stay on the manual tree-tooling path, never in tests or init.
```

- [ ] **Step 5: Add the helper needed by the lazy-load tree test**

```elisp
(defun config-smoke--tree-entrypoint-load-result ()
  (let* ((default-directory config-smoke--root-dir)
         (lisp-dir (expand-file-name "lisp" config-smoke--root-dir))
         (tree-file (expand-file-name "lisp/tools/tree.el" config-smoke--root-dir))
         (output-buffer (generate-new-buffer " *config-smoke-tree-entrypoint*"))
         (form
          `(let ((load-prefer-newer t)
                 (user-emacs-directory ,config-smoke--root-dir)
                 (entrypoint-ran nil))
             (defmacro use-package (_name &rest _args) nil)
             (provide 'use-package)
             (defun treemacs () (setq entrypoint-ran t))
             (provide 'treemacs)
             (add-to-list 'load-path ,lisp-dir)
             (load ,tree-file nil 'nomessage)
             (when (fboundp 'tools-tree-toggle)
               (tools-tree-toggle))
             (princ "RESULT ")
             (princ
              (prin1-to-string
               (list :feature (featurep 'tools-tree)
                     :entrypoint entrypoint-ran))))))
    (unwind-protect
        (let ((status (call-process "emacs" nil output-buffer nil "--batch" "-Q"
                                    "--eval" (prin1-to-string form))))
          (with-current-buffer output-buffer
            (let ((output (buffer-string))
                  (result-start nil))
              (setq result-start (string-match "RESULT " output))
              (list :status status
                    :feature (and result-start
                                  (plist-get
                                   (read (substring output
                                                    (+ result-start
                                                       (length "RESULT "))))
                                   :feature))
                    :entrypoint (and result-start
                                     (plist-get
                                      (read (substring output
                                                       (+ result-start
                                                          (length "RESULT "))))
                                      :entrypoint))))))
      (kill-buffer output-buffer))))
```

- [ ] **Step 6: Run the full verification set**

Run: `emacs --batch -Q -l init.el -l test/config-load.el -f ert-run-tests-batch-and-exit`
Expected: PASS

Run: `emacs --batch -Q -l init.el --eval '(princ "ok\n")'`
Expected: prints `ok`

Run: `emacs -Q -l init.el`
Expected: visually modern shell with `doom-one`, restrained dashboard/modeline, and no tree package setup during startup.

Run inside Emacs: `M-x tools-tree-toggle`
Expected: tree view opens on demand, `tools-tree` loads only then, and treemacs follows the active theme direction.

- [ ] **Step 7: Commit**

```bash
git add lisp/tools/tree.el lisp/editor/keys.el test/config-load.el
git commit -m "feat: align treemacs with modern ui shell"
```
