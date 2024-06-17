(use-package python
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

  ;; Configure 'black' formatter for 'apheleia'
  (setf (alist-get 'black apheleia-formatters) '("black" "-S" "-l" "120" "-"))

  :config
  (lsp-bridge-mode))
