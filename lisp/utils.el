;;; utils.el --- Self-made utilities -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defun delete-file-and-buffer ()
  "Delete current buffer and corresponding file."
  (interactive)
  (delete-file (buffer-file-name))
  (kill-buffer))

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
