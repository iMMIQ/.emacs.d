;;; editing.el --- Editing helpers -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun editor-editing-comment-line ()
  "Comment or uncomment the current line."
  (interactive)
  (comment-line 1))

(defun editor-editing-indent-buffer ()
  "Indent the current buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(provide 'editor-editing)
;;; editing.el ends here
