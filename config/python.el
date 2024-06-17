(add-hook 'python-mode-hook
          (lambda ()
            (setq lsp-bridge-python-command "python3")
            (setq lsp-bridge-python-lsp-server "pylsp")
            (setq lsp-bridge-enable-hover-diagnostic t)
            (lsp-bridge-mode)
            ;; Configure 'black' formatter for 'apheleia'
            (setf (alist-get 'black apheleia-formatters) '("black" "-S" "-l" "120" "-"))))
