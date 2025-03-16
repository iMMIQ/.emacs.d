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

(use-package posframe
  :straight t
  :defer t)

(use-package rime
  :straight t
  :config
  (setq rime-posframe-properties
	(list :background-color "#333333"
              :foreground-color "#dcdccc"
              :font "WenQuanYi Micro Hei Mono-14"
              :internal-border-width 10))
  (setq rime-user-data-dir "~/code/rime-ice")
  (setq default-input-method "rime")
  (setq rime-show-candidate 'posframe))
