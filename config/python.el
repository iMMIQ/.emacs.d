(use-package python
  :straight t
  :defer t
  :mode ("[./]flake8\\'" . conf-mode)
  :mode ("/Pipfile\\'" . conf-mode)
  :init
  (setq python-shell-interpreter "python3")
  (setq python-indent-guess-indent-offset-verbose nil)

  ;; lsp-bridge
  (setq lsp-bridge-python-command "python3")
  (setq lsp-bridge-python-lsp-server
	(cond
	 ((executable-find "pylsp") "pylsp")
	 ((executable-find "Microsoft.Python.LanguageServer") "Microsoft.Python.LanguageServer")
	 (t "pyright")))
  (setq lsp-bridge-enable-hover-diagnostic t)

  (dolist (formatter
           '(("black" . ("black" "-S" "-l" "120" "-"))
             ("yapf" . ("yapf" "--style" "{based_on_style: pep8, column_limit: 120}" "-"))
             ("autopep8" . ("autopep8" "--max-line-length" "120" "-"))))
    (when (executable-find (car formatter))
      (setf (alist-get (intern (car formatter)) apheleia-formatters) (cdr formatter))))

  :config
  (lsp-bridge-mode))

(let ((pyflakes-path (executable-find "pyflakes")))
  (when pyflakes-path
    (use-package pyimport
      :straight t
      :defer t
      :hook (python-mode . pyimport-mode)
      :config
      (setq pyimport-pyflakes-path pyflakes-path))))

(when (executable-find "isort")
  (use-package py-isort
    :straight t
    :defer t
    :hook (before-save . py-isort-before-save)
    :config
    (setq py-isort-options '("-l 120" "--profile=black"))))
