(straight-use-package 'yasnippet)

(straight-use-package
 '(lsp-bridge :host github
              :repo "manateelazycat/lsp-bridge"
              :files ("*.el" "*.py" "acm" "core" "langserver"
                      "multiserver" "resources")))

(unless (display-graphic-p)
  (straight-use-package
   '(popon :host nil :repo "https://codeberg.org/akib/emacs-popon.git"))
  (straight-use-package
   '(acm-terminal :host github :repo "twlz0ne/acm-terminal")))

(add-hook 'emacs-startup-hook
          (lambda ()
            (require 'yasnippet)
            (yas-global-mode 1)

            (require 'lsp-bridge)
            (global-lsp-bridge-mode)

            (unless (display-graphic-p)
              (with-eval-after-load 'acm
                (require 'acm-terminal)))))


;; Load python-mode when opening .py files
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;; Enable lsp-bridge when python-mode starts
(add-hook 'python-mode-hook (lambda ()
			      ;; Configure Python interpreter and pylsp language server
			      (setq lsp-bridge-python-command "python3")  ;; Specify the Python interpreter
			      (setq lsp-bridge-python-lsp-server "pylsp")     ;; Specify pylsp as the language server
                              (require 'lsp-bridge)
                              (lsp-bridge-mode)))
