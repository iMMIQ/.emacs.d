;;; display.el --- Display shell defaults -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'seq)

(defconst ui-display-font-preferences
  '("JetBrains Mono" "Iosevka Comfy" "Sarasa Mono SC")
  "Preferred font families for graphical frames.")

(defconst ui-display-font-height 140
  "Preferred default font height for graphical frames.")

(defun ui-display--apply-font-to-current-and-future-frames (font)
  "Apply FONT to the current frame and future GUI frames."
  (set-face-attribute 'default (selected-frame)
                      :font font
                      :height ui-display-font-height)
  (set-face-attribute 'default t
                      :font font
                      :height ui-display-font-height))

(defun ui-display--set-first-available-font ()
  "Set the first available preferred font for graphical frames."
  (when (display-graphic-p)
    (let ((font (seq-find (lambda (family)
                            (member family (font-family-list)))
                          ui-display-font-preferences)))
      (when font
        (ui-display--apply-font-to-current-and-future-frames font)))))

(defun ui-display-apply ()
  "Apply display shell defaults."
  (setq frame-title-format nil
        split-width-threshold 160
        split-height-threshold nil)
  (setq-default line-spacing 0.16)
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (when (display-graphic-p)
    (setq-default internal-border-width 12)
    (setq internal-border-width 12)
    (add-to-list 'default-frame-alist '(internal-border-width . 12)))
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
  (ui-display--set-first-available-font)
  (when (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
  (when (display-graphic-p)
    (set-frame-parameter (selected-frame) 'fullscreen 'maximized)))

(provide 'ui-display)
;;; display.el ends here
