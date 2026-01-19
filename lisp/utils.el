;;; utils.el --- Self-made utilities -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defun delete-file-and-buffer ()
  "Delete current buffer and corresponding file."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not filename)
        (message "Current buffer is not visiting a file!")
      (when (y-or-n-p (format "Are you sure you want to delete %s? " filename))
        (delete-file filename)
        (kill-buffer)
        (message "File '%s' deleted." filename)))))

(defun zq/org-html-bold ()
  "Emphasize with HTML's bold tag."
  (interactive)
  (let ((string "") beg end move)
    (if (org-region-active-p)
        (setq beg (region-beginning)
              end (region-end)
              string (buffer-substring beg end))
      (setq move t))
    (setq string (concat "@@html:<b>@@" string "@@html:</b>@@"))
    (when beg (delete-region beg end))
    (insert string)
    (and move (backward-char 13))))

(defun zq/org-html-span ()
  "Wrap region with <span> and </span>."
  (interactive)
  (let ((string "") beg end move)
    (if (org-region-active-p)
        (setq beg (region-beginning)
              end (region-end)
              string (buffer-substring beg end))
      (setq move t))
    (setq string (concat "@@html:<span>@@" string "@@html:</span>@@"))
    (when beg (delete-region beg end))
    (insert string)
    (and move (backward-char 16))))


(provide 'utils)

;;; utils.el ends here
