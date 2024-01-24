;;; utils.el --- Self-made utilities -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defun delete-file-and-buffer ()
  "Delete current buffer and corresponding file."
  (interactive)
  (delete-file (buffer-file-name))
  (kill-buffer))


(provide 'utils)

;;; utils.el ends here
