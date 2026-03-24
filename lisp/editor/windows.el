;;; windows.el --- Window helpers -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun editor-windows-split-below ()
  "Split the current window below and focus the new window."
  (interactive)
  (select-window (split-window-below)))

(defun editor-windows-split-right ()
  "Split the current window right and focus the new window."
  (interactive)
  (select-window (split-window-right)))

(defun editor-windows-delete-window ()
  "Delete the current window."
  (interactive)
  (delete-window))

(defun editor-windows-delete-other-windows ()
  "Delete all other windows."
  (interactive)
  (delete-other-windows))

(provide 'editor-windows)
;;; windows.el ends here
