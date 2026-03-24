(require 'ert)

(ert-deftest config-smoke/init-loads ()
  (should (featurep 'init)))

(ert-deftest config-smoke/loader-features-load ()
  (dolist (feature '(core-paths
                     core-bootstrap
                     core-performance
                     editor-evil
                     editor-keys
                     ui-theme
                     tools-completion))
    (should (featurep feature))))
