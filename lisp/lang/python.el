;;; python.el --- Python language support -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'lang-base "lang/base")

(defun lang-python--lsp-server ()
  "Return the preferred Python LSP server executable name."
  (cond
   ((executable-find "pylsp") "pylsp")
   ((executable-find "Microsoft.Python.LanguageServer")
    "Microsoft.Python.LanguageServer")
   (t "pyright")))

(defun lang-python--formatter ()
  "Return the preferred Python formatter symbol and command."
  (catch 'found
    (dolist (formatter
             '((black . ("black" "-S" "-l" "120" "-"))
               (yapf . ("yapf" "--style"
                        "{based_on_style: pep8, column_limit: 120}" "-"))
               (autopep8 . ("autopep8" "--max-line-length" "120" "-"))))
      (when (executable-find (symbol-name (car formatter)))
        (throw 'found formatter)))
    nil))

(defun lang-python--configure-apheleia ()
  "Configure Apheleia to use the preferred Python formatter."
  (let ((formatter (lang-python--formatter)))
    (when formatter
      (setf (alist-get (car formatter) apheleia-formatters)
            (cdr formatter))
      (setf (alist-get 'python-mode apheleia-mode-alist)
            (car formatter))
      (setf (alist-get 'python-ts-mode apheleia-mode-alist)
            (car formatter)))))

(with-eval-after-load 'apheleia-formatters
  (lang-python--configure-apheleia))

(use-package python
  :init
  (setq python-shell-interpreter "python3"
        python-indent-guess-indent-offset-verbose nil
        lsp-bridge-python-command "python3"
        lsp-bridge-python-lsp-server (lang-python--lsp-server))
  :mode ("\\.py\\'" . python-mode)
  :hook (python-mode . lsp-bridge-mode))

(provide 'lang-python)
;;; python.el ends here
