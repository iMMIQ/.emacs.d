(use-package verilog-mode
  :straight t
  :defer t
  :mode ("\\.v\\'" . verilog-mode)
  :mode ("\\.sv\\'" . verilog-mode)
  :mode ("\\.vh\\'" . verilog-mode)
  :init
  (setq verilog-indent-level 2)
  (setq verilog-indent-level-module 2)
  (setq verilog-indent-level-declaration 2)
  (setq verilog-indent-level-behavioral 2)
  (setq verilog-indent-level-directive 2)
  (setq verilog-case-indent 2)
  (setq verilog-auto-newline nil)

  :config
  (when (executable-find "verible-verilog-format")
    (setf (alist-get 'verible-verilog-format apheleia-formatters)
          '("verible-verilog-format" "--indentation_spaces" "2" "--column_limit" "100" "-"))
    (add-to-list 'apheleia-mode-alist '(verilog-mode . verible-verilog-format)))

  :hook (verilog-mode . (lambda ()
                          (when (executable-find "verible-verilog-ls")
                            (lsp-bridge-mode)))))
