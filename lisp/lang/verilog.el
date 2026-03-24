;;; verilog.el --- Verilog language support -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'lang-base "lang/base")

(defun lang-verilog--configure-apheleia ()
  "Register the Verible formatter when it is available."
  (when (executable-find "verible-verilog-format")
    (setf (alist-get 'verible-verilog-format apheleia-formatters)
          '("verible-verilog-format"
            "--indentation_spaces" "2"
            "--column_limit" "100"
            "-"))
    (setf (alist-get 'verilog-mode apheleia-mode-alist)
          'verible-verilog-format)))

(defun lang-verilog--maybe-enable-lsp-bridge ()
  "Enable lsp-bridge when Verible's language server is available."
  (when (executable-find "verible-verilog-ls")
    (lsp-bridge-mode 1)))

(with-eval-after-load 'apheleia-formatters
  (lang-verilog--configure-apheleia))

(use-package verilog-mode
  :straight t
  :init
  (setq verilog-indent-level 2
        verilog-indent-level-module 2
        verilog-indent-level-declaration 2
        verilog-indent-level-behavioral 2
        verilog-indent-level-directive 2
        verilog-case-indent 2
        verilog-auto-newline nil)
  :mode (("\\.v\\'" . verilog-mode)
         ("\\.sv\\'" . verilog-mode)
         ("\\.svh\\'" . verilog-mode)
         ("\\.vh\\'" . verilog-mode))
  :hook (verilog-mode . lang-verilog--maybe-enable-lsp-bridge))

(provide 'lang-verilog)
;;; verilog.el ends here
