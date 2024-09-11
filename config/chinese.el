(use-package pyim
  :straight t
  :defer t
  :init
  ;; Set the cache directory for pyim
  (setq pyim-dcache-directory (expand-file-name "pyim-dcache/" user-emacs-directory))
  ;; Use pyim as the default input method when Emacs starts
  (setq default-input-method "pyim")
  :config
  ;; Use the minibuffer to display pyim input prompts in terminal
  (setq pyim-page-tooltip 'minibuffer)

  ;; Compatibility with evil mode
  (with-eval-after-load 'evil-escape
    (defun +chinese--input-method-p ()
      (equal current-input-method "pyim"))
    (add-to-list 'evil-escape-inhibit-functions #'+chinese--input-method-p))

  ;; Use ivy as the re-builder with pyim
  (autoload 'pyim-cregexp-ivy "pyim-cregexp-utils")
  (setq ivy-re-builders-alist '((t . pyim-cregexp-ivy))))

(use-package pyim-greatdict
  :straight (pyim-greatdict :host nil :repo "https://github.com/tumashu/pyim-greatdict.git")
  :after pyim
  :config
  (let ((file (expand-file-name "straight/repos/pyim-greatdict/pyim-greatdict.pyim.gz" user-emacs-directory)))
    (if (and (featurep 'pyim) (file-exists-p file))
        (pyim-extra-dicts-add-dict
         `(:name "Greatdict-elpa"
                 :file ,file
                 :coding utf-8-unix
                 :dict-type pinyin-dict
                 :elpa t))
      (message "pyim is not available, pyim-greatdict setup failed."))))

(use-package pangu-spacing
  :straight t
  :hook (text-mode . pangu-spacing-mode)
  :config
  ;; Always insert real spaces in org-mode
  (add-hook 'org-mode-hook
            (lambda () (setq pangu-spacing-real-insert-separtor t))))

(use-package evil-pinyin
  :straight t
  :after evil
  :config
  (setq-default evil-pinyin-with-search-rule 'always)
  (global-evil-pinyin-mode 1))
