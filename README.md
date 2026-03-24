# .emacs.d

This repository contains an Emacs configuration rebuilt around a small startup
layer and feature modules under `lisp/`.

## Structure

- `early-init.el` sets early startup behavior such as GC tuning and disables
  package.el at startup.
- `init.el` is the active entry point. It adds `lisp/` to `load-path`, boots
  the core modules, loads the top-level feature set, and then tries optional
  personal modules.
- `lisp/core/` contains startup paths, bootstrap, performance, and shared
  loader helpers.
- `lisp/editor/` contains editing, window, buffer, keybinding, and Evil setup.
- `lisp/tools/` contains completion, search, project, git, code, and tree
  integrations.
- `lisp/ui/` contains startup, display, modeline, and theme modules.
- `lisp/lang/` contains shared language defaults plus Python, Rust, and Verilog
  support.
- `lisp/personal/` contains optional personal modules that should fail soft
  during startup.
- `config/` is legacy and inactive. It remains in the repository for reference
  only and is not part of the active startup path.

## Workflow

- Make active configuration changes in `lisp/` modules and keep `init.el`
  thin.
- Use `early-init.el` only for settings that must happen before normal init.
- Run the smoke suite after changes:

```sh
emacs --batch -Q -l init.el -l test/config-load.el -f ert-run-tests-batch-and-exit
```
