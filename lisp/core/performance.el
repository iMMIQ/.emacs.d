;;; performance.el --- Startup performance defaults -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(setq read-process-output-max (* 1024 1024)
      inhibit-compacting-font-caches t)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216
                  gc-cons-percentage 0.1)))

(provide 'core-performance)
;;; performance.el ends here
