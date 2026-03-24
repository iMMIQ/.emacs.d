;;; buffers.el --- Buffer helpers -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun editor-buffers-next ()
  "Switch to the next buffer."
  (interactive)
  (next-buffer))

(defun editor-buffers-previous ()
  "Switch to the previous buffer."
  (interactive)
  (previous-buffer))

(defun editor-buffers-kill-current ()
  "Kill the current buffer."
  (interactive)
  (kill-current-buffer))

(defun editor-buffers-list ()
  "Show the buffer list."
  (interactive)
  (list-buffers))

(provide 'editor-buffers)
;;; buffers.el ends here
