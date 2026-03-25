# Emacs Modern UI Refresh Design

**Date:** 2026-03-25

**Goal:** Refresh the active Emacs configuration so it looks more modern and visually cohesive, with a One Dark theme, cleaner typography, a modern modeline, a minimal startup dashboard, and UI behavior aligned with a focused, professional editing workflow.

## Current State

- The active startup path is rooted in [`init.el`](/home/ayd/.emacs.d/init.el), which loads the active modules under `lisp/`.
- The live UI shell is intentionally minimal and currently limited to:
  - startup screen suppression in [`lisp/ui/startup.el`](/home/ayd/.emacs.d/lisp/ui/startup.el)
  - menu/tool/scroll bar removal in [`lisp/ui/display.el`](/home/ayd/.emacs.d/lisp/ui/display.el)
  - a lightweight stock modeline in [`lisp/ui/modeline.el`](/home/ayd/.emacs.d/lisp/ui/modeline.el)
  - theme application with `modus-operandi` in [`lisp/ui/theme.el`](/home/ayd/.emacs.d/lisp/ui/theme.el)
- The inactive `config/ui/` tree contains older reference ideas such as `dashboard`, `doom-modeline`, and richer window styling, but those files are not loaded by the current configuration.

## Design Goals

- Use a One Dark theme as the default appearance.
- Make the UI feel modern, minimal, and professional rather than dense or flashy.
- Improve the whole visual shell, not only the theme:
  - typography
  - spacing
  - frame presentation
  - startup screen
  - modeline
  - file tree styling
- Keep the implementation localized to the active `lisp/` modules.
- Preserve startup resilience when optional UI packages or icon fonts are unavailable.

## Non-Goals

- Re-enable the inactive `config/` tree.
- Add a tab-heavy or panel-heavy workflow.
- Make visual packages mandatory for startup.
- Auto-install fonts during startup.
- Redesign unrelated editing, completion, or language features.

## Chosen Approach

The selected approach is the balanced full-shell refresh:

- Use a One Dark theme package as the visual base.
- Add a modern but restrained modeline.
- Add a minimal dashboard startup screen.
- Improve fonts, line spacing, window appearance, and frame defaults.
- Keep file-tree integration optional, but visually aligned.

This approach gives a clear visual upgrade without drifting into a noisy or novelty-focused UI.

## Target Architecture

All active changes stay inside `lisp/ui/` and optionally the active tree tooling module in `lisp/tools/`.

### [`lisp/ui/theme.el`](/home/ayd/.emacs.d/lisp/ui/theme.el)

Responsibilities:

- Define One Dark as the default theme.
- Load the theme package through the existing package/bootstrap path.
- Centralize theme application so all UI shell modules are applied before the theme is enabled.
- Gracefully fall back if the theme package is missing.

Planned behavior:

- Replace the current default `modus-operandi` with a One Dark family theme.
- Prefer `doom-one` if available because it fits the requested visual direction and works well with a modern modeline and tree tooling.
- If the package is unavailable, preserve startup by falling back to a safe built-in theme or leaving theme application unchanged.

### [`lisp/ui/display.el`](/home/ayd/.emacs.d/lisp/ui/display.el)

Responsibilities:

- Apply frame and presentation defaults.
- Configure fonts and visual spacing.
- Set global display preferences that shape the overall look.

Planned behavior:

- Keep menu bar, tool bar, and scroll bar disabled.
- Set cleaner frame defaults:
  - no noisy title text
  - maximized initial frame in GUI sessions
  - subtle internal borders or padding where supported
- Configure a modern font preference chain for GUI Emacs, with graceful fallback:
  - `JetBrains Mono`
  - `Iosevka Comfy`
  - `Sarasa Mono SC`
- Increase `line-spacing` slightly for readability.
- Prefer side-by-side splits for wide frames, but avoid heavy or surprising window automation.
- Enable useful global display defaults such as line numbers where already expected, while keeping non-code buffers visually quiet where appropriate.

### [`lisp/ui/modeline.el`](/home/ayd/.emacs.d/lisp/ui/modeline.el)

Responsibilities:

- Replace the stock modeline with a modern one.
- Keep the presentation minimal and informative rather than overloaded.

Planned behavior:

- Use `doom-modeline` if available.
- Configure it conservatively:
  - project-aware file name display
  - major mode visibility
  - position information
  - VCS status when available
  - no excessive minor-mode noise
  - no decorative clutter that weakens the minimal aesthetic
- If `doom-modeline` is unavailable, retain the current built-in modeline settings instead of failing startup.

### [`lisp/ui/startup.el`](/home/ayd/.emacs.d/lisp/ui/startup.el)

Responsibilities:

- Replace the plain startup suppression with a curated startup surface.

Planned behavior:

- Continue suppressing the stock startup screen.
- Use `dashboard` if available, configured in a restrained layout.
- Show only high-value sections such as:
  - recent files
  - projects
  - bookmarks
- Avoid a busy landing page. The dashboard should feel like a fast launch surface, not a decorative home screen.
- If `dashboard` is unavailable, keep the current startup suppression behavior.

### [`lisp/tools/tree.el`](/home/ayd/.emacs.d/lisp/tools/tree.el)

Responsibilities:

- Keep file tree support optional.
- Align tree presentation with the modern UI shell.

Planned behavior:

- Continue exposing `treemacs` as an optional command-oriented tool.
- Add light visual integration only:
  - theme compatibility
  - cleaner indentation or follow behavior where practical
  - no hard dependency on extra tree plugins
- Avoid making the file tree a required part of startup or navigation.

## Visual Direction

The target look is:

- dark, calm, low-noise
- modern and coding-focused
- more polished than stock Emacs
- less visually busy than the inactive reference configuration

The design should feel closer to a modern editor shell while still remaining recognizably Emacs.

## Package Strategy

UI enhancements are optional dependencies, not boot-critical infrastructure.

Expected package set:

- One Dark theme package
- `doom-modeline`
- `dashboard`
- icon support packages only when needed by the selected modeline or dashboard configuration

Constraints:

- Do not auto-install fonts on startup.
- Do not assume icon fonts are present.
- Prefer text fallbacks when icons cannot render.
- Avoid startup-time network or package-install side effects.

## Data Flow and Startup Flow

The UI startup sequence remains simple:

1. `init.el` loads the top-level UI module.
2. `ui-theme-apply` calls:
   - startup setup
   - display setup
   - modeline setup
   - theme activation
3. Optional UI packages activate only when present.
4. Missing optional packages degrade the experience gracefully but do not stop Emacs from starting.

This preserves the thin bootstrap model already used by the active configuration.

## Error Handling and Resilience

The UI refresh must be robust in these cases:

- running in terminal Emacs
- missing theme package
- missing `doom-modeline`
- missing `dashboard`
- missing icon fonts

Required behavior:

- no hard startup failure for missing optional UI dependencies
- GUI-only settings guarded behind display checks
- package-specific calls wrapped so absent packages fall back cleanly
- terminal sessions remain readable and functional even if the full visual shell is unavailable

## Testing Strategy

Implementation should preserve and extend the existing smoke-test posture.

Required verification goals:

- batch startup through [`test/config-load.el`](/home/ayd/.emacs.d/test/config-load.el) continues to pass
- `ui-theme-apply` succeeds in batch or non-GUI contexts
- configuration still loads when optional UI packages are absent
- startup flow remains valid when personal modules fail soft

Practical verification:

- run the existing batch smoke suite
- add or extend focused tests where needed for UI module loading behavior
- manually inspect one GUI startup after the change to confirm:
  - One Dark theme is active
  - modeline is modernized
  - dashboard is minimal and functional
  - fonts and spacing are visibly improved

## Implementation Boundaries

The planned implementation should remain focused on visual modernization only.

Allowed scope:

- `lisp/ui/*.el`
- optionally [`lisp/tools/tree.el`](/home/ayd/.emacs.d/lisp/tools/tree.el) if tree styling needs light integration
- tests that protect the UI loading path

Out of scope:

- migration of the active completion stack
- keybinding redesign
- editor behavior changes unrelated to visual presentation
- large window-management workflow changes

## Success Criteria

This work is successful when:

- Emacs starts with a One Dark appearance by default.
- The interface looks materially more modern and cohesive.
- The modeline and startup page are upgraded without becoming noisy.
- The active configuration stays modular and easy to maintain.
- Startup remains resilient when optional UI packages or fonts are unavailable.
