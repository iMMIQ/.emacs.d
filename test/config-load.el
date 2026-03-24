(require 'ert)

(ert-deftest config-smoke/startup-skeleton-loads ()
  (dolist (feature '(core-paths
                     core-bootstrap
                     core-performance
                     editor-evil
                     editor-keys
                     ui-theme
                     tools-completion))
    (should (featurep feature))))

(ert-deftest config-smoke/init-loads ()
  (should (featurep 'init)))
