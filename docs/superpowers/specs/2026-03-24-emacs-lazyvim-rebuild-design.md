# Emacs LazyVim-Style Rebuild Design

**Date:** 2026-03-24

**Goal:** Rebuild this Emacs configuration into a cleaner, faster, LazyVim-inspired setup centered on `evil`, `SPC` leader workflows, modern minibuffer tooling, and a clearer module architecture.

## Current Problems

- Startup concerns, package bootstrap, and regular configuration are mixed in [`init.el`](/home/ayd/.emacs.d/init.el).
- `early-init.el` is missing, so startup optimizations and UI suppression happen too late.
- Some modules are dead or partially dead. [`config/ui/windows.el`](/home/ayd/.emacs.d/config/ui/windows.el) exists but is never loaded.
- The current stack mixes older choices (`ivy`, `counsel`, `projectile`) with newer behavior goals (LazyVim-style motions and leader keys), which creates friction and duplicated behavior.
- There are repeated or conflicting settings, including line number setup and `winum`.
- Emacs 30 compatibility debt exists, including obsolete advice style.
- Personal modules and startup-time side effects are not isolated from the main editing path.

## Design Goals

- Keep `evil` as the editing model.
- Keep `SPC` as the global leader key.
- Make daily interaction feel closer to LazyVim without attempting a literal plugin-for-plugin clone.
- Replace the old completion and project stack with a more native Emacs 30-friendly stack.
- Keep `lsp-bridge` for language intelligence.
- Keep `magit` and `apheleia`.
- Reduce startup work and remove install-time side effects from the normal boot path.
- Make module boundaries explicit so future maintenance is predictable.

## Non-Goals

- Preserve all existing keybindings.
- Preserve the current file layout under `config/`.
- Migrate to `eglot`.
- Keep UI packages that do not support the new workflow cleanly, especially if they add startup cost without enough benefit.

## Target Architecture

The configuration will be reorganized around startup phase and responsibility.

### Root Files

- `early-init.el`
  - Startup-only settings that must apply before package initialization and frame setup.
  - GC tuning for startup.
  - Temporary `file-name-handler-alist` minimization.
  - `package-enable-at-startup` disable.
  - Basic frame UI suppression.

- `init.el`
  - Minimal bootstrap entry.
  - Adds `lisp/` to the load path.
  - Loads a small number of top-level modules in a deterministic order.

### `lisp/` Layout

- `lisp/core/`
  - package bootstrap
  - shared helpers
  - persistence paths and `custom.el`
  - startup and runtime performance helpers

- `lisp/editor/`
  - `evil`
  - leader and local leader key infrastructure
  - window and buffer navigation
  - editing helpers such as pairing and commenting

- `lisp/ui/`
  - theme
  - modeline
  - dashboard
  - font and icon setup
  - popup and display behavior

- `lisp/tools/`
  - minibuffer completion stack
  - search
  - project management
  - git integration
  - shell and terminal tools
  - file tree as optional tooling

- `lisp/lang/`
  - language-specific support only
  - mode activation
  - formatter registration
  - `lsp-bridge` integration
  - mode-local keymaps if needed

- `lisp/personal/`
  - input method
  - music
  - any machine-specific or personal workflows

## Load Order

1. `early-init.el`
2. `core`
3. `editor`
4. `ui`
5. `tools`
6. `lang`
7. `personal`

This order ensures key infrastructure is available before feature packages start defining commands and bindings.

## Target Technology Stack

### Keep

- `evil`
- `general`
- `which-key`
- `lsp-bridge`
- `apheleia`
- `magit`
- `diff-hl`
- `treemacs` as optional file tree tooling

### Replace

- `ivy`, `counsel`, `ivy-rich`
  - Replace with `vertico`, `orderless`, `marginalia`, `consult`, `embark`, `embark-consult`

- `projectile`, `counsel-projectile`
  - Replace with built-in `project.el` plus a thin helper layer if project UX gaps remain

- custom shell-pipeline-based search functions
  - Replace with `consult-ripgrep`, `consult-line`, `consult-imenu`, and project-aware search wrappers

- `centaur-tabs`
  - Remove from the core workflow

### Likely Remove or Downgrade

- auto-install behavior for icon fonts and theme packages during startup
- duplicated `winum` declarations
- any obsolete advice fallback kept only for old Emacs versions

## LazyVim-Style Interaction Model

The goal is semantic alignment, not literal parity.

### Global Rules

- `SPC` is the leader in normal state.
- `SPC m` is local leader for mode-specific actions.
- `SPC l` stays empty for now.
- `SPC c` is the single top-level code-management group, including LSP-related actions.

### Planned Leader Groups

- `SPC f`
  - file actions
- `SPC b`
  - buffer actions
- `SPC p`
  - project actions
- `SPC g`
  - git actions
- `SPC s`
  - search and navigation actions
- `SPC c`
  - code and LSP actions
- `SPC w`
  - window actions
- `SPC t`
  - toggles
- `SPC q`
  - quit and session actions

### Planned Non-Leader Navigation

- `gd`
  - definition
- `gD`
  - definition in other window
- `gr`
  - references
- `gi`
  - implementation
- `gt`
  - type definition
- `K`
  - documentation
- `[d`
  - previous diagnostic
- `]d`
  - next diagnostic

### Planned `SPC c` Tree

- `SPC c a`
  - code action
- `SPC c r`
  - rename
- `SPC c f`
  - format
- `SPC c d`
  - diagnostics list
- `SPC c n`
  - next diagnostic
- `SPC c p`
  - previous diagnostic
- `SPC c s`
  - workspace symbols
- `SPC c S`
  - symbol at point
- `SPC c l`
  - `lsp-bridge` lifecycle operations such as restart or toggle

## Module-Specific Design Notes

### Completion and Search

- `vertico` will provide the candidate UI.
- `orderless` will provide matching semantics.
- `marginalia` will enrich annotations.
- `consult` becomes the standard entry point for file search, buffer switching, line search, project grep, and structural navigation.
- `embark` will provide action menus instead of custom minibuffer action glue.

This removes a large amount of custom search code and shell coupling.

### Projects

- Use built-in `project.el` as the primary project abstraction.
- Wrap common commands in stable helper functions so keybindings do not depend directly on package internals.
- Avoid reintroducing `projectile` unless the rebuilt workflow exposes a real gap that native project support cannot cover.

### UI

- Theme loading becomes explicit and side-effect free.
- Fonts and icon packages are configured but never installed automatically during startup.
- Modeline stays, but only if the selected package is lightweight enough in practice.
- Dashboard can remain if its startup cost is acceptable after the rebuild; otherwise it should be reduced or removed.

### Windows and Tree

- Window management becomes keyboard-first and leader-driven.
- `treemacs` remains optional and does not define the core navigation model.
- Tabs are removed from the default workflow.

### Language Support

- `lsp-bridge` remains the LSP backend.
- Each language module is responsible only for its mode hooks, formatter setup, and `lsp-bridge` entry.
- Shared code actions and navigation bindings live in editor or tools modules, not inside individual language files.

### Personal Modules

- Personal modules move behind clear boundaries and later loading where practical.
- Startup must stay healthy even if a personal tool such as `rime` or `emms` is missing.

## Migration Strategy

The rebuild should happen in two implementation phases.

### Phase 1: Foundation

- Add `early-init.el`.
- Replace `init.el` with a minimal loader.
- Create the `lisp/` hierarchy.
- Move package bootstrap and core startup logic.
- Rebuild `evil`, leader keys, and core window and buffer actions.
- Replace the completion, search, and project stack.
- Rebuild git integration.

### Phase 2: Language and Personal Modules

- Reconnect `lsp-bridge`.
- Rebuild Python, Rust, and Verilog integration.
- Reconnect optional tree and UI extras.
- Migrate personal modules.
- Remove the old `config/` tree once the new tree is stable.

## Risks

- `lsp-bridge` completion behavior may overlap awkwardly with the new minibuffer and in-buffer completion stack and will need explicit boundaries.
- Some old keybinding habits will break by design.
- Built-in `project.el` may expose gaps relative to `projectile` for edge-case workflows.
- Terminal and GUI behavior may diverge if popup or icon packages are not carefully scoped.

## Validation Requirements

- `emacs --batch -Q -l init.el` must load cleanly.
- No obsolete advice warnings in Emacs 30.
- Core commands must resolve and load:
  - file finding
  - buffer switching
  - project switching
  - ripgrep search
  - magit status
  - `lsp-bridge` navigation
- GUI startup must not trigger automatic font installation.
- Terminal startup must remain functional.

## Acceptance Criteria

- The configuration structure is rebuilt under `lisp/` with `early-init.el` present.
- `ivy`, `counsel`, `ivy-rich`, and `projectile` are removed from the active configuration.
- `SPC` leader groups are rebuilt around the new semantics, with `SPC c` as the sole code-management group and `SPC l` unused.
- `lsp-bridge` remains functional for existing language modules.
- The old configuration tree is either removed or left only as clearly inactive transitional material.
