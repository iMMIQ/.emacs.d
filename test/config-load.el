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

(ert-deftest config-smoke/startup-skeleton-loads ()
  (config-smoke--ensure-init-loaded)
  (dolist (feature (append '(core-paths
                             core-bootstrap
                             core-performance)
                           core-bootstrap-top-level-features))
    (should (featurep feature))))

(ert-deftest config-smoke/init-loads ()
  (config-smoke--ensure-init-loaded)
  (should (featurep 'init)))

(ert-deftest config-smoke/leader-functions-exist ()
  (config-smoke--ensure-init-loaded)
  (should (fboundp 'emacs-leader))
  (should (fboundp 'emacs-local-leader)))

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
