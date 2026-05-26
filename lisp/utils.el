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

(defun zq/org--initial-metadata-end ()
  "Return position after initial Org keywords and property drawers."
  (save-excursion
    (goto-char (point-min))
    (let ((last (point-min))
          continue)
      (setq continue t)
      (while continue
        ;; Allow blank lines between metadata blocks, but do not make
        ;; the final blank line part of the insertion point.
        (while (looking-at-p "^[ \t]*$")
          (forward-line 1))

        (cond
         ;; Org file keyword, e.g. #+title:, #+author:, #+options:
         ((looking-at-p "^[ \t]*#\\+[A-Za-z0-9_]+:")
          (forward-line 1)
          (setq last (point)))

         ;; Initial property drawer.
         ((looking-at-p "^[ \t]*:PROPERTIES:[ \t]*$")
          (if (re-search-forward "^[ \t]*:END:[ \t]*$" nil t)
              (progn
                (forward-line 1)
                (setq last (point)))
            ;; Malformed drawer; stop instead of scanning unpredictably.
            (setq continue nil)))

         (t
          (setq continue nil))))
      last)))

(defun zq/org-add-bigblow-setupfile ()
  "Add Bigblow SETUPFILE after initial Org metadata if absent."
  (interactive)
  (let* ((url "https://fniessen.github.io/org-html-themes/org/html-theme-bigblow.setup")
         (line (concat "#+setupfile: " url))
         (existing (cdr (assoc "SETUPFILE"
                               (org-collect-keywords '("SETUPFILE"))))))
    (unless (member url existing)
      (save-excursion
        (goto-char (zq/org--initial-metadata-end))
        (insert line "\n")))))


(provide 'utils)

;;; utils.el ends here
