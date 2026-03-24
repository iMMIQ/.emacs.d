(require 'ert)

(defconst config-smoke--root-dir
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Root directory for the startup skeleton smoke tests.")

(defun config-smoke--ensure-init-loaded ()
  "Load the startup skeleton if it is not already loaded."
  (unless (featurep 'init)
    (let ((user-emacs-directory config-smoke--root-dir))
      (load (expand-file-name "init.el" config-smoke--root-dir)
            nil
            'nomessage))))

(defun config-smoke--leader-binding (state keys)
  "Return the command bound to KEYS in STATE within the leader map."
  (lookup-key general-override-mode-map
              (vconcat (vector state) (kbd keys))))

(ert-deftest config-smoke/startup-skeleton-loads ()
  (config-smoke--ensure-init-loaded)
  (dolist (feature (append '(core-paths
                             core-bootstrap
                             core-performance)
                           core-bootstrap-top-level-features))
    (should (featurep feature))))

(ert-deftest config-smoke/display-features-load ()
  (config-smoke--ensure-init-loaded)
  (should (featurep 'ui-display))
  (should (featurep 'ui-theme)))

(ert-deftest config-smoke/ui-theme-load-is-side-effect-free ()
  (let* ((default-directory config-smoke--root-dir)
         (theme-file
          (expand-file-name "lisp/ui/theme.el" config-smoke--root-dir))
         (lisp-dir (expand-file-name "lisp" config-smoke--root-dir))
         (output-buffer (generate-new-buffer " *config-smoke-ui-theme*"))
         (form
          `(let ((load-prefer-newer t)
                 (inhibit-startup-screen 'sentinel-startup)
                 (custom-enabled-themes nil)
                 (menu-bar-mode 'sentinel-menu)
                 (tool-bar-mode 'sentinel-tool)
                 (scroll-bar-mode 'sentinel-scroll)
                 (line-number-mode 'sentinel-line)
                 (column-number-mode 'sentinel-column)
                 (size-indication-mode 'sentinel-size))
             (add-to-list 'load-path ,lisp-dir)
             (load ,theme-file nil 'nomessage)
             (princ
              (prin1-to-string
               (list :theme (featurep 'ui-theme)
                     :display (featurep 'ui-display)
                     :startup inhibit-startup-screen
                     :themes custom-enabled-themes
                     :menu menu-bar-mode
                     :tool tool-bar-mode
                     :scroll scroll-bar-mode
                     :line line-number-mode
                     :column column-number-mode
                     :size size-indication-mode))))))
    (unwind-protect
        (let ((status (call-process "emacs"
                                    nil
                                    output-buffer
                                    nil
                                    "--batch"
                                    "-Q"
                                    "--eval"
                                    (prin1-to-string form))))
          (should (equal status 0))
          (with-current-buffer output-buffer
            (pcase-let ((`(:theme ,theme
                           :display ,display
                           :startup ,startup
                           :themes ,themes
                           :menu ,menu
                           :tool ,tool
                           :scroll ,scroll
                           :line ,line
                           :column ,column
                           :size ,size)
                         (read (buffer-string))))
              (should theme)
              (should display)
              (should (eq startup 'sentinel-startup))
              (should (null themes))
              (should (eq menu 'sentinel-menu))
              (should (eq tool 'sentinel-tool))
              (should (eq scroll 'sentinel-scroll))
              (should (eq line 'sentinel-line))
              (should (eq column 'sentinel-column))
              (should (eq size 'sentinel-size)))))
      (kill-buffer output-buffer))))

(ert-deftest config-smoke/init-loads ()
  (config-smoke--ensure-init-loaded)
  (should (featurep 'init)))

(ert-deftest config-smoke/leader-functions-exist ()
  (config-smoke--ensure-init-loaded)
  (should (fboundp 'emacs-leader))
  (should (fboundp 'emacs-local-leader)))

(ert-deftest config-smoke/completion-commands-exist ()
  (config-smoke--ensure-init-loaded)
  (should (fboundp 'consult-find))
  (should (fboundp 'consult-buffer))
  (should (fboundp 'embark-act)))

(ert-deftest config-smoke/project-helpers-exist ()
  (config-smoke--ensure-init-loaded)
  (should (fboundp 'my/project-find-file))
  (should (fboundp 'my/project-switch))
  (should (fboundp 'my/project-search)))

(ert-deftest config-smoke/project-leader-bindings-exist ()
  (config-smoke--ensure-init-loaded)
  (dolist (state '(normal-state visual-state motion-state))
    (should (eq (config-smoke--leader-binding state "SPC p f")
                #'my/project-find-file))
    (should (eq (config-smoke--leader-binding state "SPC p p")
                #'my/project-switch))
    (should (eq (config-smoke--leader-binding state "SPC p s")
                #'my/project-search))))

(ert-deftest config-smoke/project-loads-with-search-dependency ()
  (let* ((default-directory config-smoke--root-dir)
         (project-file
          (expand-file-name "lisp/tools/project.el" config-smoke--root-dir))
         (lisp-dir (expand-file-name "lisp" config-smoke--root-dir))
         (consult-dir
          (expand-file-name "straight/build/consult" config-smoke--root-dir))
         (output-buffer (generate-new-buffer " *config-smoke-project*"))
         (form
          `(progn
             (add-to-list 'load-path ,consult-dir)
             (add-to-list 'load-path ,lisp-dir)
             (load ,project-file nil 'nomessage)
             (princ (if (and (fboundp 'my/project-search)
                             (fboundp 'consult-ripgrep))
                        "ok"
                      "missing")))))
    (unwind-protect
        (let ((status (call-process "emacs"
                                    nil
                                    output-buffer
                                    nil
                                    "--batch"
                                    "-Q"
                                    "--eval"
                                    (prin1-to-string form))))
          (should (equal status 0))
          (with-current-buffer output-buffer
            (should (string-match-p "ok" (buffer-string)))))
      (kill-buffer output-buffer))))

(ert-deftest config-smoke/git-and-code-bindings-exist ()
  (config-smoke--ensure-init-loaded)
  (should (fboundp 'magit-status))
  (should (fboundp 'apheleia-format))
  (should (fboundp 'lsp-bridge-code-action)))

(ert-deftest config-smoke/git-and-code-leader-bindings-exist ()
  (config-smoke--ensure-init-loaded)
  (dolist (state '(normal-state visual-state motion-state))
    (should (eq (config-smoke--leader-binding state "SPC g s")
                #'magit-status))
    (should (eq (config-smoke--leader-binding state "SPC c a")
                #'lsp-bridge-code-action))
    (should (eq (config-smoke--leader-binding state "SPC c r")
                #'lsp-bridge-rename))
    (should (eq (config-smoke--leader-binding state "SPC c f")
                #'apheleia-format))
    (should (eq (config-smoke--leader-binding state "SPC c d")
                #'lsp-bridge-diagnostic-list))
    (should (eq (config-smoke--leader-binding state "SPC c n")
                #'lsp-bridge-diagnostic-jump-next))
    (should (eq (config-smoke--leader-binding state "SPC c p")
                #'lsp-bridge-diagnostic-jump-prev))
    (should (eq (config-smoke--leader-binding state "SPC c l")
                #'lsp-bridge-restart-process))))

(ert-deftest config-smoke/git-tools-refresh-diff-hl-after-magit ()
  (config-smoke--ensure-init-loaded)
  (require 'magit)
  (should (memq #'diff-hl-magit-post-refresh magit-post-refresh-hook)))

(ert-deftest config-smoke/completion-loads-outside-user-emacs-directory ()
  (let* ((fake-user-emacs-directory (make-temp-file "config-smoke-emacs-dir" t))
         (default-directory config-smoke--root-dir)
         (completion-file
          (expand-file-name "lisp/tools/completion.el" config-smoke--root-dir))
         (lisp-dir (expand-file-name "lisp" config-smoke--root-dir))
         (output-buffer (generate-new-buffer " *config-smoke-completion*"))
         (form
          `(let ((user-emacs-directory ,fake-user-emacs-directory))
             (add-to-list 'load-path ,lisp-dir)
             (load ,completion-file nil 'nomessage)
             (princ (if (and (fboundp 'consult-buffer)
                             (fboundp 'embark-act))
                        "ok"
                      "missing")))))
    (unwind-protect
        (let ((status (call-process "emacs"
                                    nil
                                    output-buffer
                                    nil
                                    "--batch"
                                    "-Q"
                                    "--eval"
                                    (prin1-to-string form))))
          (should (equal status 0))
          (with-current-buffer output-buffer
            (should (string-match-p "ok" (buffer-string)))))
      (kill-buffer output-buffer)
      (delete-directory fake-user-emacs-directory t))))

(ert-deftest config-smoke/which-key-loads-from-repo-build ()
  (config-smoke--ensure-init-loaded)
  (let ((which-key-file (locate-library "which-key")))
    (should which-key-file)
    (should (string-match-p "/straight/build/which-key/" which-key-file))))

(ert-deftest config-smoke/top-level-feature-mapping-is-explicit ()
  (config-smoke--ensure-init-loaded)
  (should (equal (core-bootstrap-feature-path 'editor-evil)
                 "editor/evil"))
  (should (equal (core-bootstrap-feature-path 'editor-keys)
                 "editor/keys"))
  (should-not (core-bootstrap-feature-path 'config-smoke-fake-module)))

(ert-deftest config-smoke/loader-prefers-real-modules ()
  (config-smoke--ensure-init-loaded)
  (let* ((feature 'config-smoke-real-module)
         (temp-dir (make-temp-file "config-smoke-real-module" t))
         (module-file (expand-file-name "config-smoke-real-module.el" temp-dir))
         (load-path (cons temp-dir load-path)))
    (unwind-protect
        (progn
          (with-temp-file module-file
            (insert "(setq config-smoke-real-module-loaded t)\n"
                    "(provide 'config-smoke-real-module)\n"))
          (core-lib-require-feature feature)
          (should (featurep feature))
          (should (bound-and-true-p config-smoke-real-module-loaded)))
      (when (featurep feature)
        (unload-feature feature t))
      (when (boundp 'config-smoke-real-module-loaded)
        (makunbound 'config-smoke-real-module-loaded))
      (delete-directory temp-dir t))))
