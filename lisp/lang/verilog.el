;;; verilog.el --- Verilog language support -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'lang-base "lang/base")

(use-package verilog-mode
  :mode (("\\.v\\'" . verilog-mode)
         ("\\.sv\\'" . verilog-mode)
         ("\\.svh\\'" . verilog-mode)
         ("\\.vh\\'" . verilog-mode))
  :hook (verilog-mode . lsp-bridge-mode))

(provide 'lang-verilog)
;;; verilog.el ends here
