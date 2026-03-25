(require 'cl-lib)
(require 'ert)

(defconst config-smoke--root-dir
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Root directory for the startup skeleton smoke tests.")

(defun config-smoke--read-file (path)
  "Return the contents of PATH relative to the repo root."
  (with-temp-buffer
    (insert-file-contents (expand-file-name path config-smoke--root-dir))
    (buffer-string)))

(defun config-smoke--path-under-directory-p (path directory)
  "Return non-nil when PATH resolves inside DIRECTORY."
  (let* ((directory-root (file-name-as-directory
                          (expand-file-name directory config-smoke--root-dir)))
         (directory-name (directory-file-name directory-root))
         (candidates
          (delete-dups
           (list (expand-file-name path)
                 (expand-file-name path config-smoke--root-dir)))))
    (cl-some
     (lambda (candidate)
       (or (string= candidate directory-name)
           (string-prefix-p directory-root candidate)))
     candidates)))

(defun config-smoke--paths-under-directory (paths directory)
  "Return every entry from PATHS that resolves inside DIRECTORY."
  (cl-remove-if-not
   (lambda (path)
     (and (stringp path)
          (config-smoke--path-under-directory-p path directory)))
   paths))

(defun config-smoke--loaded-files-under-directory (directory)
  "Return loaded files recorded in `load-history' under DIRECTORY."
  (let (files)
    (dolist (entry load-history (nreverse files))
      (let ((file (car entry)))
        (when (and (stringp file)
                   (config-smoke--path-under-directory-p file directory))
          (push file files))))))

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

(defun config-smoke--expected-python-lsp-server ()
  "Return the Python LSP server expected from the migrated config."
  (cond
   ((executable-find "pylsp") "pylsp")
   ((executable-find "Microsoft.Python.LanguageServer")
    "Microsoft.Python.LanguageServer")
   (t "pyright")))

(defun config-smoke--expected-python-formatter ()
  "Return the expected Python formatter symbol and command, or nil."
  (catch 'found
    (dolist (formatter
             '((black . ("black" "-S" "-l" "120" "-"))
               (yapf . ("yapf" "--style"
                        "{based_on_style: pep8, column_limit: 120}" "-"))
               (autopep8 . ("autopep8" "--max-line-length" "120" "-"))))
      (when (executable-find (symbol-name (car formatter)))
        (throw 'found formatter)))
    nil))

(defun config-smoke--personal-module-load-result (module-file feature package)
  "Return the subprocess load result for MODULE-FILE providing FEATURE.
PACKAGE names the optional package whose setup should be forced to fail."
  (let* ((default-directory config-smoke--root-dir)
         (lisp-dir (expand-file-name "lisp" config-smoke--root-dir))
         (output-buffer (generate-new-buffer " *config-smoke-personal-module*"))
         (form
          `(let ((load-prefer-newer t))
             (defmacro use-package (name &rest _args)
               (if (eq name ',package)
                   (signal 'error
                           (list (format "simulated setup failure for %s"
                                         name)))
                 nil))
             (provide 'use-package)
             (add-to-list 'load-path ,lisp-dir)
             (load ,module-file nil 'nomessage)
             (princ "RESULT ")
             (princ
              (prin1-to-string
               (list :feature (featurep ',feature)))))))
    (unwind-protect
        (let ((status (call-process "emacs"
                                    nil
                                    output-buffer
                                    nil
                                    "--batch"
                                    "-Q"
                                    "--eval"
                                    (prin1-to-string form)))
              (output (with-current-buffer output-buffer
                        (buffer-string)))
              (result-start nil))
          (setq result-start (string-match "RESULT " output))
          (list :status status
                :output output
                :data (and result-start
                           (read (substring output
                                            (+ result-start
                                               (length "RESULT ")))))))
      (kill-buffer output-buffer)
      )))

(defun config-smoke--init-load-result-with-personal-failures ()
  "Return the subprocess result for init when personal setup fails."
  (let* ((default-directory config-smoke--root-dir)
         (init-file (expand-file-name "init.el" config-smoke--root-dir))
         (output-buffer (generate-new-buffer " *config-smoke-init-personal*"))
         (form
          `(let ((user-emacs-directory ,config-smoke--root-dir)
                 (load-prefer-newer t))
             (require 'cl-lib)
             (let ((real-eval (symbol-function 'eval)))
               (cl-letf (((symbol-function 'eval)
                          (lambda (form &optional lexical)
                            (if (and (consp form)
                                     (eq (car form) 'use-package)
                                     (memq (cadr form) '(rime emms)))
                                (signal 'error
                                        (list (format "simulated setup failure for %s"
                                                      (cadr form))))
                              (funcall real-eval form lexical)))))
                 (load ,init-file nil 'nomessage)))
             (princ "RESULT ")
             (princ
              (prin1-to-string
               (list :init (featurep 'init)
                     :chinese (featurep 'personal-chinese)
                     :music (featurep 'personal-music)
                     :theme (featurep 'ui-theme)))))))
    (unwind-protect
        (let ((status (call-process "emacs"
                                    nil
                                    output-buffer
                                    nil
                                    "--batch"
                                    "-Q"
                                    "--eval"
                                    (prin1-to-string form)))
              (output (with-current-buffer output-buffer
                        (buffer-string)))
              (result-start nil))
          (setq result-start (string-match "RESULT " output))
          (list :status status
                :output output
                :data (and result-start
                           (read (substring output
                                            (+ result-start
                                               (length "RESULT ")))))))
      (kill-buffer output-buffer)
      )))

(defun config-smoke--init-load-result-with-ui-package-failures ()
  "Return the subprocess result for init when optional UI packages fail."
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
                     :theme (featurep 'ui-theme)
                     :themes custom-enabled-themes))))))
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

(defun config-smoke--real-init-theme-load-result ()
  "Return the subprocess result for init using the repo's real theme path."
  (let* ((default-directory config-smoke--root-dir)
         (init-file (expand-file-name "init.el" config-smoke--root-dir))
         (output-buffer (generate-new-buffer " *config-smoke-real-init-theme*"))
         (form
          '(progn
             (princ "RESULT ")
             (princ
              (prin1-to-string
               (list :init (featurep 'init)
                     :theme (featurep 'ui-theme)
                     :themes custom-enabled-themes))))))
    (unwind-protect
        (let ((status (call-process "emacs"
                                    nil
                                    output-buffer
                                    nil
                                    "--batch"
                                    "-Q"
                                    "--eval"
                                    (prin1-to-string
                                     `(setq user-emacs-directory
                                            ,config-smoke--root-dir))
                                    "-l"
                                    init-file
                                    "--eval"
                                    (prin1-to-string form))))
          (with-current-buffer output-buffer
            (let* ((output (buffer-string))
                   (result-start (string-match "RESULT " output)))
              (list :status status
                    :data (and result-start
                               (read (substring output
                                                (+ result-start
                                                   (length "RESULT ")))))
                    :output output))))
      (kill-buffer output-buffer))))

(defun config-smoke--ui-theme-load-result-with-package-theme-failure ()
  "Return the subprocess result when package-backed `doom-one' fails to load.
The subprocess simulates `doom-themes' being present while the first
`doom-one' load fails, so the repo-local fallback should be used."
  (let* ((default-directory config-smoke--root-dir)
         (lisp-dir (expand-file-name "lisp" config-smoke--root-dir))
         (theme-file (expand-file-name "lisp/ui/theme.el" config-smoke--root-dir))
         (local-theme-dir
          (expand-file-name "lisp/themes" config-smoke--root-dir))
         (output-buffer (generate-new-buffer " *config-smoke-theme-fallback*"))
         (form
         `(let ((load-prefer-newer t)
                 (user-emacs-directory ,config-smoke--root-dir)
                 (custom-enabled-themes nil)
                 (custom-theme-load-path nil)
                 (theme-load-attempts nil))
             (require 'cl-lib)
             (defun ui-startup-apply ())
             (defun ui-display-apply ())
             (defun ui-modeline-apply ())
             (provide 'ui-startup)
             (provide 'ui-display)
             (provide 'ui-modeline)
             (provide 'doom-themes)
             (add-to-list 'load-path ,lisp-dir)
             (cl-letf (((symbol-function 'load-theme)
                        (lambda (theme &optional _no-confirm _no-enable)
                          (push (list theme (copy-sequence custom-theme-load-path))
                                theme-load-attempts)
                          (cond
                           ((eq theme 'doom-one)
                            (if (member ,local-theme-dir custom-theme-load-path)
                                (progn
                                  (setq custom-enabled-themes '(doom-one))
                                  t)
                              (signal 'error '("simulated package doom-one failure"))))
                           ((eq theme 'deeper-blue)
                            (setq custom-enabled-themes '(deeper-blue))
                            t)
                           (t
                            (signal 'error (list "unexpected theme" theme))))))
                       ((symbol-function 'disable-theme)
                        (lambda (_theme) nil)))
               (load ,theme-file nil 'nomessage)
               (ui-theme-apply)
               (princ "RESULT ")
               (princ
                (prin1-to-string
                 (list :feature (featurep 'ui-theme)
                       :themes custom-enabled-themes
                       :custom-theme-load-path custom-theme-load-path
                       :attempts (nreverse theme-load-attempts))))))))
    (unwind-protect
        (let ((status (call-process "emacs" nil output-buffer nil "--batch" "-Q"
                                    "--eval" (prin1-to-string form))))
          (with-current-buffer output-buffer
            (let ((output (buffer-string))
                  (result-start nil))
              (setq result-start (string-match "RESULT " output))
              (list :status status
                    :output output
                    :data (and result-start
                               (read (substring output
                                                (+ result-start
                                                   (length "RESULT ")))))))))
      (kill-buffer output-buffer))))

(defun config-smoke--tree-entrypoint-load-result ()
  "Return the subprocess load result for the lazy tree entrypoint."
  (let* ((default-directory config-smoke--root-dir)
         (lisp-dir (expand-file-name "lisp" config-smoke--root-dir))
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
             (autoload 'tools-tree-toggle "tools/tree" nil t)
             (let ((feature-before (featurep 'tools-tree)))
               (when (fboundp 'tools-tree-toggle)
                 (tools-tree-toggle))
               (princ "RESULT ")
               (princ
                (prin1-to-string
                 (list :feature-before feature-before
                       :feature (featurep 'tools-tree)
                       :entrypoint entrypoint-ran)))))))
    (unwind-protect
        (let ((status (call-process "emacs" nil output-buffer nil "--batch" "-Q"
                                    "--eval" (prin1-to-string form))))
          (with-current-buffer output-buffer
            (let ((output (buffer-string))
                  (result-start nil))
              (setq result-start (string-match "RESULT " output))
              (list :status status
                    :output output
                    :data (and result-start
                               (read (substring output
                                                (+ result-start
                                                   (length "RESULT ")))))))))
      (kill-buffer output-buffer))))

(defun config-smoke--tree-entrypoint-load-result-with-tools-tree-load-failure ()
  "Return the subprocess result when loading `tools/tree.el' setup fails."
  (let* ((default-directory config-smoke--root-dir)
         (lisp-dir (expand-file-name "lisp" config-smoke--root-dir))
         (output-buffer
          (generate-new-buffer " *config-smoke-tree-entrypoint-load-failure*"))
         (form
          `(let ((load-prefer-newer t)
                 (user-emacs-directory ,config-smoke--root-dir))
             (defmacro use-package (name &rest _args)
               (if (eq name 'treemacs)
                   (signal 'error '("simulated treemacs setup failure"))
                 nil))
             (provide 'use-package)
             (add-to-list 'load-path ,lisp-dir)
             (autoload 'tools-tree-toggle "tools/tree" nil t)
             (tools-tree-toggle)
             (princ "RESULT ")
             (princ
              (prin1-to-string
               (list :feature (featurep 'tools-tree)
                     :treemacs-bound (fboundp 'treemacs)))))))
    (unwind-protect
        (let ((status (call-process "emacs" nil output-buffer nil "--batch" "-Q"
                                    "--eval" (prin1-to-string form))))
          (with-current-buffer output-buffer
            (let ((output (buffer-string))
                  (result-start nil))
              (setq result-start (string-match "RESULT " output))
              (list :status status
                    :output output
                    :data (and result-start
                               (read (substring output
                                                (+ result-start
                                                   (length "RESULT ")))))))))
      (kill-buffer output-buffer))))

(defun config-smoke--tree-entrypoint-load-result-without-treemacs ()
  "Return the subprocess result when treemacs is unavailable."
  (let* ((default-directory config-smoke--root-dir)
         (lisp-dir (expand-file-name "lisp" config-smoke--root-dir))
         (output-buffer
          (generate-new-buffer " *config-smoke-tree-entrypoint-missing*"))
         (form
          `(let ((load-prefer-newer t)
                 (user-emacs-directory ,config-smoke--root-dir))
             (defmacro use-package (_name &rest _args) nil)
             (provide 'use-package)
             (add-to-list 'load-path ,lisp-dir)
             (autoload 'tools-tree-toggle "tools/tree" nil t)
             (tools-tree-toggle)
             (princ "RESULT ")
             (princ
              (prin1-to-string
               (list :feature (featurep 'tools-tree)
                     :treemacs-bound (fboundp 'treemacs)))))))
    (unwind-protect
        (let ((status (call-process "emacs" nil output-buffer nil "--batch" "-Q"
                                    "--eval" (prin1-to-string form))))
          (with-current-buffer output-buffer
            (let ((output (buffer-string))
                  (result-start nil))
              (setq result-start (string-match "RESULT " output))
              (list :status status
                    :output output
                    :data (and result-start
                               (read (substring output
                                                (+ result-start
                                                   (length "RESULT ")))))))))
      (kill-buffer output-buffer))))

(defun config-smoke--ui-module-load-result
    (module-path expected-feature apply-function missing-features)
  "Return the subprocess load result for a UI MODULE-PATH.
EXPECTED-FEATURE is checked after loading, APPLY-FUNCTION is invoked,
and MISSING-FEATURES are simulated as unavailable `require' targets."
  (let* ((default-directory config-smoke--root-dir)
         (lisp-dir (expand-file-name "lisp" config-smoke--root-dir))
         (full-path (expand-file-name module-path config-smoke--root-dir))
         (output-buffer (generate-new-buffer " *config-smoke-ui-module*"))
         (form
          `(let ((load-prefer-newer t))
             (require 'cl-lib)
             (defmacro use-package (name &rest args)
               (let (forms)
                 (while args
                   (let ((keyword (pop args))
                         (value (pop args)))
                     (when (memq keyword '(:preface :init :config))
                       (setq forms
                             (append forms
                                     (if (and (listp value)
                                              (eq (car-safe value) 'progn))
                                         (cdr value)
                                       (list value)))))))
                 (cons 'progn
                       (cons (list 'require (list 'quote name))
                             forms))))
             (provide 'use-package)
             (add-to-list 'load-path ,lisp-dir)
             (setq line-number-mode nil
                   column-number-mode nil
                   size-indication-mode nil
                   inhibit-startup-screen nil)
             (let ((real-require (symbol-function 'require)))
               (cl-letf (((symbol-function 'require)
                          (lambda (feature &optional filename noerror)
                            (if (memq feature ',missing-features)
                                (if noerror nil (signal 'error (list feature)))
                              (funcall real-require feature filename noerror)))))
                 (load ,full-path nil 'nomessage)
                 (funcall #',apply-function)
                 (princ "RESULT ")
                 (princ
                  (prin1-to-string
                   (list :feature (featurep ',expected-feature)
                         :line-number line-number-mode
                         :column-number column-number-mode
                         :size-indication size-indication-mode
                         :startup-screen inhibit-startup-screen))))))))
    (unwind-protect
        (let ((status (call-process "emacs"
                                    nil
                                    output-buffer
                                    nil
                                    "--batch"
                                    "-Q"
                                    "--eval"
                                    (prin1-to-string form))))
          (with-current-buffer output-buffer
            (let ((output (buffer-string))
                  (result-start nil))
              (setq result-start (string-match "RESULT " output))
              (list :status status
                    :output output
                    :data (and result-start
                               (read (substring output
                                                (+ result-start
                                                   (length "RESULT ")))))))))
      (kill-buffer output-buffer))))

(defun config-smoke--ui-module-load-result-with-stubs
    (module-path expected-feature apply-function stub-features icon-capable-form)
  "Return the subprocess load result for a UI MODULE-PATH with stub packages.
EXPECTED-FEATURE is checked after loading, APPLY-FUNCTION is invoked,
STUB-FEATURES maps features to forms evaluated before the apply function,
and ICON-CAPABLE-FORM defines the shared icon capability probe."
  (let* ((default-directory config-smoke--root-dir)
         (lisp-dir (expand-file-name "lisp" config-smoke--root-dir))
         (full-path (expand-file-name module-path config-smoke--root-dir))
         (output-buffer (generate-new-buffer " *config-smoke-ui-module-stubs*"))
         (form
          `(let ((load-prefer-newer t))
             (add-to-list 'load-path ,lisp-dir)
             (setq line-number-mode nil
                   column-number-mode nil
                   size-indication-mode nil
                   inhibit-startup-screen nil
                   doom-modeline-height nil
                   doom-modeline-bar-width nil
                   doom-modeline-buffer-file-name-style nil
                   doom-modeline-minor-modes t
                   doom-modeline-buffer-encoding t
                   doom-modeline-indent-info t
                   doom-modeline-icon t
                   dashboard-banner-logo-title nil
                   dashboard-startup-banner nil
                   dashboard-center-content nil
                   dashboard-show-shortcuts t
                   dashboard-footer-messages t
                   dashboard-set-heading-icons t
                   dashboard-set-file-icons t
                   dashboard-items nil
                   dashboard-setup-startup-hook-count 0
                   doom-modeline-mode-call-count 0
                   ui-startup--dashboard-hook-installed nil)
             (defun ui-icon-capable-p () ,icon-capable-form)
             ,@(mapcar #'cdr stub-features)
             (load ,full-path nil 'nomessage)
             (funcall #',apply-function)
             (princ "RESULT ")
             (princ
              (prin1-to-string
               (list :feature (featurep ',expected-feature)
                     :line-number line-number-mode
                     :column-number column-number-mode
                     :size-indication size-indication-mode
                     :startup-screen inhibit-startup-screen
                     :doom-modeline-height doom-modeline-height
                     :doom-modeline-bar-width doom-modeline-bar-width
                     :doom-modeline-buffer-file-name-style
                     doom-modeline-buffer-file-name-style
                     :doom-modeline-minor-modes doom-modeline-minor-modes
                     :doom-modeline-buffer-encoding doom-modeline-buffer-encoding
                     :doom-modeline-indent-info doom-modeline-indent-info
                     :doom-modeline-icon doom-modeline-icon
                     :dashboard-banner-logo-title dashboard-banner-logo-title
                     :dashboard-startup-banner dashboard-startup-banner
                     :dashboard-center-content dashboard-center-content
                     :dashboard-show-shortcuts dashboard-show-shortcuts
                     :dashboard-footer-messages dashboard-footer-messages
                     :dashboard-set-heading-icons dashboard-set-heading-icons
                     :dashboard-set-file-icons dashboard-set-file-icons
                     :dashboard-items dashboard-items
                     :dashboard-hook-count dashboard-setup-startup-hook-count
                     :dashboard-hook-installed ui-startup--dashboard-hook-installed
                     :doom-modeline-mode-call-count doom-modeline-mode-call-count))))))
    (unwind-protect
        (let ((status (call-process "emacs"
                                    nil
                                    output-buffer
                                    nil
                                    "--batch"
                                    "-Q"
                                    "--eval"
                                    (prin1-to-string form))))
          (with-current-buffer output-buffer
            (let ((output (buffer-string))
                  (result-start nil))
              (setq result-start (string-match "RESULT " output))
              (list :status status
                    :output output
                    :data (and result-start
                               (read (substring output
                                                (+ result-start
                                                   (length "RESULT ")))))))))
      (kill-buffer output-buffer))))

(ert-deftest config-smoke/startup-skeleton-loads ()
  (config-smoke--ensure-init-loaded)
  (dolist (feature (append '(core-paths
                             core-bootstrap
                             core-performance)
                           core-bootstrap-top-level-features))
    (should (featurep feature))))

(ert-deftest config-smoke/tree-module-stays-optional-at-startup ()
  (config-smoke--ensure-init-loaded)
  (should-not (featurep 'tools-tree))
  (should-not (memq 'tools-tree core-bootstrap-top-level-features)))

(ert-deftest config-smoke/personal-chinese-module-loads-when-rime-setup-fails ()
  (let* ((result
         (config-smoke--personal-module-load-result
           (expand-file-name "lisp/personal/chinese.el" config-smoke--root-dir)
           'personal-chinese
           'rime))
         (status (plist-get result :status)))
    (should (equal status 0))
    (pcase-let ((`(:feature ,feature)
                 (plist-get result :data)))
      (should feature))))

(ert-deftest config-smoke/personal-music-module-loads-when-emms-setup-fails ()
  (let* ((result
         (config-smoke--personal-module-load-result
           (expand-file-name "lisp/personal/music.el" config-smoke--root-dir)
           'personal-music
           'emms))
         (status (plist-get result :status)))
    (should (equal status 0))
    (pcase-let ((`(:feature ,feature)
                 (plist-get result :data)))
      (should feature))))

(ert-deftest config-smoke/init-loads-when-personal-setup-fails ()
  (let* ((result (config-smoke--init-load-result-with-personal-failures))
         (status (plist-get result :status)))
    (should (equal status 0))
    (pcase-let ((`(:init ,init
                   :chinese ,chinese
                   :music ,music
                   :theme ,theme)
                 (plist-get result :data)))
      (should init)
      (should chinese)
      (should music)
      (should theme))))

(ert-deftest config-smoke/init-loads-when-optional-ui-packages-are-missing ()
  (let ((result (config-smoke--init-load-result-with-ui-package-failures)))
    (should (equal (plist-get result :status) 0))
    (pcase-let ((`(:init ,init :theme ,theme :themes ,themes)
                 (plist-get result :data)))
      (should init)
      (should theme)
      (should (equal themes '(doom-one))))))

(ert-deftest config-smoke/ui-theme-falls-back-to-repo-doom-one-when-package-theme-load-fails ()
  (let ((result (config-smoke--ui-theme-load-result-with-package-theme-failure)))
    (should (equal (plist-get result :status) 0))
    (pcase-let ((`(:feature ,feature
                   :themes ,themes
                   :custom-theme-load-path ,theme-load-path
                   :attempts ,attempts)
                 (plist-get result :data)))
      (should feature)
      (should (equal themes '(doom-one)))
      (should (member (expand-file-name "lisp/themes" config-smoke--root-dir)
                      theme-load-path))
      (should (equal (mapcar #'car attempts) '(doom-one doom-one)))
      (should-not (memq 'deeper-blue (mapcar #'car attempts))))))

(ert-deftest config-smoke/modeline-setup-stays-safe-without-doom-modeline ()
  (let ((result (config-smoke--ui-module-load-result
                 "lisp/ui/modeline.el"
                 'ui-modeline
                 'ui-modeline-apply
                 '(doom-modeline nerd-icons))))
    (should (equal (plist-get result :status) 0))
    (let ((data (plist-get result :data)))
      (should (plist-get data :feature))
      (should (plist-get data :line-number))
      (should (plist-get data :column-number))
      (should (plist-get data :size-indication)))))

(ert-deftest config-smoke/modeline-configures-doom-modeline-when-available ()
  (let* ((stubs
          '((doom-modeline
             .
             (progn
               (defun doom-modeline-mode (&optional _arg))
               (provide 'doom-modeline)))))
         (result (config-smoke--ui-module-load-result-with-stubs
                  "lisp/ui/modeline.el"
                  'ui-modeline
                  'ui-modeline-apply
                  stubs
                  nil)))
    (should (equal (plist-get result :status) 0))
    (let ((data (plist-get result :data)))
      (should (plist-get data :feature))
      (should (plist-get data :line-number))
      (should (plist-get data :column-number))
      (should (plist-get data :size-indication))
      (should (equal (plist-get data :doom-modeline-height) 24))
      (should (equal (plist-get data :doom-modeline-bar-width) 3))
      (should (eq (plist-get data :doom-modeline-buffer-file-name-style)
                  'truncate-upto-project))
      (should-not (plist-get data :doom-modeline-minor-modes))
      (should-not (plist-get data :doom-modeline-buffer-encoding))
      (should-not (plist-get data :doom-modeline-indent-info))
      (should-not (plist-get data :doom-modeline-icon)))))

(ert-deftest config-smoke/modeline-uses-plain-text-fallback-when-icon-check-errors ()
  (let* ((stubs
          '((doom-modeline
             .
             (progn
               (defun doom-modeline-mode (&optional _arg))
               (provide 'doom-modeline)))))
         (result (config-smoke--ui-module-load-result-with-stubs
                  "lisp/ui/modeline.el"
                  'ui-modeline
                  'ui-modeline-apply
                  stubs
                  '(error "icon probe failed"))))
    (should (equal (plist-get result :status) 0))
    (let ((data (plist-get result :data)))
      (should (plist-get data :feature))
      (should-not (plist-get data :doom-modeline-icon)))))

(ert-deftest config-smoke/modeline-stays-safe-when-doom-modeline-mode-errors ()
  (let* ((stubs
          '((doom-modeline
             .
             (progn
               (defun doom-modeline-mode (&optional _arg)
                 (setq doom-modeline-mode-call-count
                       (1+ doom-modeline-mode-call-count))
                 (error "mode activation failed"))
               (provide 'doom-modeline)))))
         (result (config-smoke--ui-module-load-result-with-stubs
                  "lisp/ui/modeline.el"
                  'ui-modeline
                  'ui-modeline-apply
                  stubs
                  nil)))
    (should (equal (plist-get result :status) 0))
    (let ((data (plist-get result :data)))
      (should (plist-get data :feature))
      (should (plist-get data :line-number))
      (should (plist-get data :column-number))
      (should (plist-get data :size-indication))
      (should (equal (plist-get data :doom-modeline-mode-call-count) 1))
      (should-not (plist-get data :doom-modeline-icon)))))

(ert-deftest config-smoke/startup-setup-stays-safe-without-dashboard ()
  (let ((result (config-smoke--ui-module-load-result
                 "lisp/ui/startup.el"
                 'ui-startup
                 'ui-startup-apply
                 '(dashboard nerd-icons))))
    (should (equal (plist-get result :status) 0))
    (let ((data (plist-get result :data)))
      (should (plist-get data :feature))
      (should (plist-get data :startup-screen)))))

(ert-deftest config-smoke/startup-configures-dashboard-when-available ()
  (let* ((stubs
          '((dashboard
             .
             (progn
               (defun dashboard-setup-startup-hook ()
                 (setq dashboard-setup-startup-hook-count
                       (1+ dashboard-setup-startup-hook-count)))
               (provide 'dashboard)))))
         (result (config-smoke--ui-module-load-result-with-stubs
                  "lisp/ui/startup.el"
                  'ui-startup
                  'ui-startup-apply
                  stubs
                  nil)))
    (should (equal (plist-get result :status) 0))
    (let ((data (plist-get result :data)))
      (should (plist-get data :feature))
      (should (plist-get data :startup-screen))
      (should (equal (plist-get data :dashboard-banner-logo-title) "Emacs"))
      (should (eq (plist-get data :dashboard-startup-banner) 'official))
      (should (plist-get data :dashboard-center-content))
      (should-not (plist-get data :dashboard-show-shortcuts))
      (should-not (plist-get data :dashboard-footer-messages))
      (should-not (plist-get data :dashboard-set-heading-icons))
      (should-not (plist-get data :dashboard-set-file-icons))
      (should (equal (plist-get data :dashboard-items)
                     '((recents . 6) (projects . 5) (bookmarks . 4))))
      (should (equal (plist-get data :dashboard-hook-count) 1)))))

(ert-deftest config-smoke/startup-keeps-dashboard-hook-one-time-and-icons-safe-on-error ()
  (let* ((stubs
          '((dashboard
             .
             (progn
               (defun dashboard-setup-startup-hook ()
                 (setq dashboard-setup-startup-hook-count
                       (1+ dashboard-setup-startup-hook-count)))
               (provide 'dashboard)))))
         (result (config-smoke--ui-module-load-result-with-stubs
                  "lisp/ui/startup.el"
                  'ui-startup
                  '(lambda ()
                     (ui-startup-apply)
                     (ui-startup-apply))
                  stubs
                  '(error "icon probe failed"))))
    (should (equal (plist-get result :status) 0))
    (let ((data (plist-get result :data)))
      (should (plist-get data :feature))
      (should (plist-get data :startup-screen))
      (should-not (plist-get data :dashboard-set-heading-icons))
      (should-not (plist-get data :dashboard-set-file-icons))
      (should (equal (plist-get data :dashboard-hook-count) 1)))))

(ert-deftest config-smoke/startup-stays-safe-when-dashboard-hook-errors ()
  (let* ((stubs
          '((dashboard
             .
             (progn
               (defun dashboard-setup-startup-hook ()
                 (setq dashboard-setup-startup-hook-count
                       (1+ dashboard-setup-startup-hook-count))
                 (error "dashboard hook failed"))
               (provide 'dashboard)))))
         (result (config-smoke--ui-module-load-result-with-stubs
                  "lisp/ui/startup.el"
                  'ui-startup
                  'ui-startup-apply
                  stubs
                  nil)))
    (should (equal (plist-get result :status) 0))
    (let ((data (plist-get result :data)))
      (should (plist-get data :feature))
      (should (plist-get data :startup-screen))
      (should (equal (plist-get data :dashboard-hook-count) 1))
      (should-not (plist-get data :dashboard-hook-installed))
      (should-not (plist-get data :dashboard-set-heading-icons))
      (should-not (plist-get data :dashboard-set-file-icons)))))

(ert-deftest config-smoke/display-features-load ()
  (config-smoke--ensure-init-loaded)
  (should (featurep 'ui-display))
  (should (featurep 'ui-theme)))

(ert-deftest config-smoke/ui-theme-default-is-doom-one ()
  (config-smoke--ensure-init-loaded)
  (should (eq ui-theme-default 'doom-one)))

(ert-deftest config-smoke/display-defaults-are-modern-but-scoped ()
  (config-smoke--ensure-init-loaded)
  (should (equal frame-title-format nil))
  (should (< (abs (- line-spacing 0.16)) 0.0001))
  (should (equal (default-value 'line-spacing) 0.16))
  (should (memq #'display-line-numbers-mode prog-mode-hook))
  (should-not (bound-and-true-p global-display-line-numbers-mode)))

(ert-deftest config-smoke/display-gui-defaults-add-subtle-padding ()
  (let ((default-frame-alist nil)
        (internal-border-width 0))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _frame) t))
              ((symbol-function 'font-family-list) (lambda () nil)))
      (ui-display-apply))
    (should (equal internal-border-width 12))
    (should (equal (alist-get 'internal-border-width default-frame-alist) 12))))

(ert-deftest config-smoke/display-gui-defaults-update-current-and-future-frames ()
  (let ((default-frame-alist nil)
        (fullscreen-call nil))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _frame) t))
              ((symbol-function 'font-family-list) (lambda () nil))
              ((symbol-function 'set-frame-parameter)
               (lambda (frame parameter value)
                 (setq fullscreen-call (list frame parameter value)))))
      (ui-display-apply))
    (should (equal (alist-get 'fullscreen default-frame-alist) 'maximized))
    (should (equal fullscreen-call
                   (list (selected-frame) 'fullscreen 'maximized)))))

(ert-deftest config-smoke/display-font-selection-updates-current-and-future-faces ()
  (let ((font-calls nil))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _frame) t))
              ((symbol-function 'font-family-list)
               (lambda () '("Monaco" "JetBrains Mono")))
              ((symbol-function 'set-face-attribute)
               (lambda (&rest args)
                 (when (eq (car args) 'default)
                   (push args font-calls)))))
      (ui-display-apply))
    (should (equal (nreverse font-calls)
                   (list
                    (list 'default (selected-frame)
                          :font "JetBrains Mono" :height 140)
                    '(default t :font "JetBrains Mono" :height 140))))))

(ert-deftest config-smoke/language-features-load ()
  (config-smoke--ensure-init-loaded)
  (should (featurep 'lang-base))
  (should (featurep 'lang-python))
  (should (featurep 'lang-rust))
  (should (featurep 'lang-verilog)))

(ert-deftest config-smoke/language-base-does-not-enable-apheleia-globally ()
  (config-smoke--ensure-init-loaded)
  (require 'apheleia)
  (should (boundp 'apheleia-global-mode))
  (should-not apheleia-global-mode))

(ert-deftest config-smoke/language-base-applies-shared-lsp-bridge-defaults ()
  (config-smoke--ensure-init-loaded)
  (should (boundp 'lsp-bridge-enable-log))
  (should (boundp 'lsp-bridge-enable-hover-diagnostic))
  (should (boundp 'lsp-bridge-enable-signature-help))
  (should (boundp 'lsp-bridge-completion-stop-when-inserting))
  (should-not lsp-bridge-enable-log)
  (should lsp-bridge-enable-hover-diagnostic)
  (should lsp-bridge-enable-signature-help)
  (should lsp-bridge-completion-stop-when-inserting))

(ert-deftest config-smoke/language-base-loads-outside-user-emacs-directory ()
  (let* ((fake-user-emacs-directory (make-temp-file "config-smoke-lang-dir" t))
         (default-directory config-smoke--root-dir)
         (base-file
          (expand-file-name "lisp/lang/base.el" config-smoke--root-dir))
         (lisp-dir (expand-file-name "lisp" config-smoke--root-dir))
         (script-file (make-temp-file "config-smoke-lang-base" nil ".el"))
         (output-buffer (generate-new-buffer " *config-smoke-lang-base*"))
         (form
          `(let ((user-emacs-directory ,fake-user-emacs-directory))
             (add-to-list 'load-path ,lisp-dir)
             (load ,base-file nil 'nomessage)
             (princ
              (prin1-to-string
               (list :feature (featurep 'lang-base)
                     :straight (and (boundp 'straight-base-dir)
                                    straight-base-dir)))))))
    (with-temp-file script-file
      (insert (prin1-to-string form)))
    (unwind-protect
        (let ((status (call-process "emacs"
                                    nil
                                    output-buffer
                                    nil
                                    "--batch"
                                    "-Q"
                                    "-l"
                                    script-file)))
          (should (equal status 0))
          (with-current-buffer output-buffer
            (pcase-let ((`(:feature ,feature
                           :straight ,straight-base-dir)
                         (read (buffer-string))))
              (should feature)
              (should (boundp 'straight-base-dir))
              (should-not (file-equal-p straight-base-dir
                                        fake-user-emacs-directory)))))
      (kill-buffer output-buffer)
      (delete-file script-file)
      (delete-directory fake-user-emacs-directory t))))

(ert-deftest config-smoke/python-language-module-carries-over-lsp-and-formatting ()
  (config-smoke--ensure-init-loaded)
  (require 'apheleia)
  (should (boundp 'python-shell-interpreter))
  (should (boundp 'python-indent-guess-indent-offset-verbose))
  (should (boundp 'lsp-bridge-python-command))
  (should (boundp 'lsp-bridge-python-lsp-server))
  (should (equal python-shell-interpreter "python3"))
  (should-not python-indent-guess-indent-offset-verbose)
  (should (equal lsp-bridge-python-command "python3"))
  (should (member '("[./]flake8\\'" . conf-mode) auto-mode-alist))
  (should (member '("/Pipfile\\'" . conf-mode) auto-mode-alist))
  (should (equal lsp-bridge-python-lsp-server
                 (config-smoke--expected-python-lsp-server)))
  (let ((formatter (config-smoke--expected-python-formatter)))
    (when formatter
      (should (equal (alist-get 'python-mode apheleia-mode-alist)
                     (car formatter)))
      (should (equal (alist-get 'python-ts-mode apheleia-mode-alist)
                     (car formatter)))
      (should (equal (alist-get (car formatter) apheleia-formatters)
                     (cdr formatter))))))

(ert-deftest config-smoke/python-language-module-enables-optional-tools-when-available ()
  (let* ((temp-dir (make-temp-file "config-smoke-python-tools" t))
         (default-directory config-smoke--root-dir)
         (python-file
          (expand-file-name "lisp/lang/python.el" config-smoke--root-dir))
         (lisp-dir (expand-file-name "lisp" config-smoke--root-dir))
         (fake-bin (expand-file-name "bin" temp-dir))
         (pyflakes (expand-file-name "pyflakes" fake-bin))
         (isort (expand-file-name "isort" fake-bin))
         (pyimport-el (expand-file-name "pyimport.el" temp-dir))
         (py-isort-el (expand-file-name "py-isort.el" temp-dir))
         (output-buffer (generate-new-buffer " *config-smoke-python-tools*"))
         (form
          `(let ((load-prefer-newer t)
                 (exec-path (cons ,fake-bin exec-path))
                 (process-environment
                  (cons (concat "PATH=" ,fake-bin path-separator
                                (or (getenv "PATH") ""))
                        process-environment)))
             (add-to-list 'load-path ,temp-dir)
             (add-to-list 'load-path ,lisp-dir)
             (load ,python-file nil 'nomessage)
             (princ
              (prin1-to-string
               (list :pyimport (memq #'pyimport-mode python-mode-hook)
                     :pyflakes (and (boundp 'pyimport-pyflakes-path)
                                    pyimport-pyflakes-path)
                     :isort (memq #'py-isort-before-save before-save-hook)
                     :options (and (boundp 'py-isort-options)
                                   py-isort-options)))))))
    (make-directory fake-bin)
    (with-temp-file pyflakes
      (insert "#!/bin/sh\nexit 0\n"))
    (set-file-modes pyflakes #o755)
    (with-temp-file isort
      (insert "#!/bin/sh\nexit 0\n"))
    (set-file-modes isort #o755)
    (with-temp-file pyimport-el
      (insert "(defvar pyimport-pyflakes-path nil)\n"
              "(defun pyimport-mode ())\n"
              "(provide 'pyimport)\n"))
    (with-temp-file py-isort-el
      (insert "(defvar py-isort-options nil)\n"
              "(defun py-isort-before-save ())\n"
              "(provide 'py-isort)\n"))
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
            (pcase-let ((`(:pyimport ,pyimport
                           :pyflakes ,pyflakes-path
                           :isort ,isort-hook
                           :options ,options)
                         (read (buffer-string))))
              (should pyimport)
              (should (equal pyflakes-path pyflakes))
              (should isort-hook)
              (should (equal options '("-l 120" "--profile=black"))))))
      (kill-buffer output-buffer)
      (delete-directory temp-dir t))))

(ert-deftest config-smoke/rust-language-module-carries-over-rustic-settings ()
  (config-smoke--ensure-init-loaded)
  (require 'rustic)
  (should (null rustic-lsp-client))
  (should-not (memq #'rustic-setup-lsp rustic-mode-hook))
  (should-not (memq #'flycheck-mode rustic-mode-hook))
  (should-not (memq #'flymake-mode-off rustic-mode-hook))
  (should (memq #'lsp-bridge-mode rustic-mode-hook))
  (should rustic-indent-method-chain)
  (should (null rustic-babel-format-src-block))
  (should (null rustic-format-trigger)))

(ert-deftest config-smoke/verilog-language-module-carries-over-formatting-and-lsp ()
  (config-smoke--ensure-init-loaded)
  (require 'apheleia)
  (require 'verilog-mode)
  (should (= verilog-indent-level 2))
  (should (= verilog-indent-level-module 2))
  (should (= verilog-indent-level-declaration 2))
  (should (= verilog-indent-level-behavioral 2))
  (should (= verilog-indent-level-directive 2))
  (should (= verilog-case-indent 2))
  (should-not verilog-auto-newline)
  (should-not (memq #'lsp-bridge-mode verilog-mode-hook))
  (let ((enabled nil)
        (real-executable-find (symbol-function 'executable-find)))
    (cl-letf (((symbol-function 'lsp-bridge-mode)
               (lambda (&optional _arg)
                 (setq enabled t)))
              ((symbol-function 'executable-find)
               (lambda (name)
                 (unless (equal name "verible-verilog-ls")
                   (funcall real-executable-find name)))))
      (run-hooks 'verilog-mode-hook))
    (should-not enabled))
  (when (executable-find "verible-verilog-format")
    (should (equal (alist-get 'verilog-mode apheleia-mode-alist)
                   'verible-verilog-format))
    (should (equal (alist-get 'verible-verilog-format apheleia-formatters)
                   '("verible-verilog-format"
                     "--indentation_spaces" "2"
                     "--column_limit" "100"
                     "-")))))

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

(ert-deftest config-smoke/ui-theme-apply-is-idempotent ()
  (let* ((default-directory config-smoke--root-dir)
         (theme-file
          (expand-file-name "lisp/ui/theme.el" config-smoke--root-dir))
         (lisp-dir (expand-file-name "lisp" config-smoke--root-dir))
         (output-buffer (generate-new-buffer " *config-smoke-ui-theme-idempotent*"))
         (form
          `(let ((load-prefer-newer t)
                 (custom-enabled-themes '(wombat)))
             (require 'cl-lib)
             (add-to-list 'load-path ,lisp-dir)
             (load ,theme-file nil 'nomessage)
             (let ((real-require (symbol-function 'require)))
               (cl-letf (((symbol-function 'require)
                          (lambda (feature &optional filename noerror)
                            (if (eq feature 'doom-themes)
                                'doom-themes
                              (funcall real-require feature filename noerror))))
                         ((symbol-function 'load-theme)
                          (lambda (theme &optional _no-confirm _no-enable)
                            (setq custom-enabled-themes (list theme)))))
                 (ui-theme-apply)
                 (ui-theme-apply)))
             (princ (prin1-to-string custom-enabled-themes)))))
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
            (should (equal (read (buffer-string))
                           '(doom-one)))))
      (kill-buffer output-buffer))))

(ert-deftest config-smoke/ui-theme-falls-back-when-doom-one-load-fails ()
  (let* ((default-directory config-smoke--root-dir)
         (theme-file
          (expand-file-name "lisp/ui/theme.el" config-smoke--root-dir))
         (lisp-dir (expand-file-name "lisp" config-smoke--root-dir))
         (output-buffer (generate-new-buffer " *config-smoke-ui-theme-fallback*"))
         (form
          `(let ((load-prefer-newer t)
                 (custom-enabled-themes '(wombat)))
             (require 'cl-lib)
             (add-to-list 'load-path ,lisp-dir)
             (load ,theme-file nil 'nomessage)
             (let ((real-require (symbol-function 'require)))
               (cl-letf (((symbol-function 'require)
                          (lambda (feature &optional filename noerror)
                            (if (eq feature 'doom-themes)
                                'doom-themes
                              (funcall real-require feature filename noerror))))
                         ((symbol-function 'load-theme)
                          (lambda (theme &optional _no-confirm _no-enable)
                            (if (eq theme 'doom-one)
                                (signal 'error '("simulated doom-one failure"))
                              (setq custom-enabled-themes (list theme))))))
                 (ui-theme-apply)))
             (princ (prin1-to-string custom-enabled-themes)))))
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
            (should (equal (read (buffer-string))
                           '(deeper-blue)))))
      (kill-buffer output-buffer))))

(ert-deftest config-smoke/bootstrap-ui-apply-is-tolerant ()
  (let* ((default-directory config-smoke--root-dir)
         (lisp-dir (expand-file-name "lisp" config-smoke--root-dir))
         (output-buffer (generate-new-buffer " *config-smoke-bootstrap-ui*"))
         (form
          `(let ((load-prefer-newer t))
             (add-to-list 'load-path ,lisp-dir)
             (require 'core-bootstrap "core/bootstrap")
             (when (fboundp 'ui-theme-apply)
               (fmakunbound 'ui-theme-apply))
             (core-bootstrap-apply-top-level-feature 'ui-theme)
             (princ "ok"))))
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

(ert-deftest config-smoke/init-applies-ui-through-bootstrap ()
  (let* ((default-directory config-smoke--root-dir)
         (init-file (expand-file-name "init.el" config-smoke--root-dir))
         (child-dir (make-temp-file "config-smoke-init-ui-cwd" t))
         (child-home (make-temp-file "config-smoke-init-ui-home" t))
         (output-buffer (generate-new-buffer " *config-smoke-init-ui*"))
         (setup-form
          `(progn
             (setq user-emacs-directory ,config-smoke--root-dir)
             (defalias 'config-smoke--real-require (symbol-function 'require))
             (defun config-smoke--require-doom-themes (feature &optional filename noerror)
               (if (eq feature 'doom-themes)
                   'doom-themes
                 (funcall 'config-smoke--real-require feature filename noerror)))
             (fset 'require #'config-smoke--require-doom-themes)
             (defun config-smoke--load-theme (theme &optional _no-confirm _no-enable)
               (setq custom-enabled-themes (list theme)))
             (fset 'load-theme #'config-smoke--load-theme)))
         (form
          '(princ
            (prin1-to-string
             (list :startup inhibit-startup-screen
                   :themes custom-enabled-themes
                   :line line-number-mode
                   :column column-number-mode
                   :size size-indication-mode)))))
    (unwind-protect
        (let ((default-directory child-dir)
              (process-environment
               (append (list (concat "HOME=" child-home))
                       process-environment)))
          (let ((status (call-process "emacs"
                                      nil
                                      output-buffer
                                      nil
                                      "--batch"
                                      "-Q"
                                      "--eval"
                                      (prin1-to-string setup-form)
                                      "-l"
                                      init-file
                                      "--eval"
                                      (prin1-to-string form))))
            (should (equal status 0))
            (with-current-buffer output-buffer
              (let ((output (buffer-string)))
                (should (string-match-p "(:startup t\\_>" output))
                (should (string-match-p ":themes (doom-one)" output))
                (should (string-match-p ":line t\\_>" output))
                (should (string-match-p ":column t\\_>" output))
                (should (string-match-p ":size t\\_>" output))))))
      (kill-buffer output-buffer)
      (delete-directory child-dir t)
      (delete-directory child-home t))))

(ert-deftest config-smoke/init-loads-real-doom-one-theme-from-repo ()
  (let ((result (config-smoke--real-init-theme-load-result)))
    (should (equal (plist-get result :status) 0))
    (pcase-let ((`(:init ,init :theme ,theme :themes ,themes)
                 (plist-get result :data)))
      (should init)
      (should theme)
      (should (equal themes '(doom-one))))))

(ert-deftest config-smoke/init-loads ()
  (config-smoke--ensure-init-loaded)
  (should (featurep 'init)))

(ert-deftest config-smoke/new-layout-is-active ()
  (config-smoke--ensure-init-loaded)
  (let* ((legacy-config-dir (expand-file-name "config" config-smoke--root-dir))
         (legacy-load-path
          (config-smoke--paths-under-directory load-path legacy-config-dir))
         (legacy-loaded-files
          (config-smoke--loaded-files-under-directory legacy-config-dir))
         (readme (config-smoke--read-file "README.md")))
    (should (featurep 'init))
    (should-not legacy-load-path)
    (should-not legacy-loaded-files)
    (should (string-match-p "^## Structure$" readme))
    (should (string-match-p "`early-init\\.el`" readme))
    (should (string-match-p "`init\\.el`" readme))
    (should (string-match-p "`lisp/`" readme))
    (should (string-match-p "`config/`" readme))
    (should (string-match-p "legacy\\|inactive" readme))))

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

(ert-deftest config-smoke/tree-entrypoint-loads-module-on-demand ()
  (let ((result (config-smoke--tree-entrypoint-load-result)))
    (should (equal (plist-get result :status) 0))
    (pcase-let ((`(:feature-before ,feature-before
                   :feature ,feature
                   :entrypoint ,entrypoint)
                 (plist-get result :data)))
      (should-not feature-before)
      (should feature)
      (should entrypoint))))

(ert-deftest config-smoke/tree-entrypoint-stays-safe-without-treemacs ()
  (let ((result (config-smoke--tree-entrypoint-load-result-without-treemacs)))
    (should (equal (plist-get result :status) 0))
    (pcase-let ((`(:feature ,feature
                   :treemacs-bound ,treemacs-bound)
                 (plist-get result :data)))
      (should feature)
      (should-not treemacs-bound))))

(ert-deftest config-smoke/tree-entrypoint-stays-safe-when-tools-tree-load-fails ()
  (let ((result
         (config-smoke--tree-entrypoint-load-result-with-tools-tree-load-failure)))
    (should (equal (plist-get result :status) 0))
    (pcase-let ((`(:feature ,feature
                   :treemacs-bound ,treemacs-bound)
                 (plist-get result :data)))
      (should feature)
      (should-not treemacs-bound))))

(ert-deftest config-smoke/tree-entrypoint-is-bound-after-init ()
  (config-smoke--ensure-init-loaded)
  (should (eq (config-smoke--leader-binding 'normal-state "SPC e")
              'tools-tree-toggle)))

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
