;;; commandline.el --- Commandline tools -*- lexical-binding: t -*-
;;; Commentary:
;; Use Emacs buffer to monitor commandline programs.
;;; Code:


(require 'comint)

;; process filter function for colored output
(require 'ansi-color)
(defun ansi-filter (proc string)
  "Filter STRING from the output of PROC by ansi color codes."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
	(save-excursion
	  ;; Insert the text, advancing the process marker.
	  (goto-char (process-mark proc))
	  (insert (ansi-color-apply string))
	  (set-marker (process-mark proc) (point)))
	(if moving (goto-char (process-mark proc)))))))


;; clash
(defun start-clash ()
  "Start clash subprocess if there isn't one."
  (interactive)
  (unless (process-live-p (get-process "<process:clash>"))
    (let ((buf (get-buffer-create "*clash*")))
      (with-current-buffer buf
	(comint-mode)
	(display-line-numbers-mode -1))
      (set-process-filter
       (start-process "<process:clash>" buf "/usr/local/bin/clash")
       #'ansi-filter))))


;; mpv
(defun play-with-mpv (path)
  "Play media specified by PATH with MPV."
  (interactive "fMedia File: ")
  (let* ((name (concat "mpv: " path))
	 (buf (get-buffer-create name)))
    (with-current-buffer buf
      (comint-mode)
      (display-line-numbers-mode -1))
    (set-process-filter
     (start-process name buf "/opt/homebrew/bin/mpv"
		    "--quiet" (expand-file-name path))
     #'ansi-filter)))


;; pyfmt
(defun format-python-code (path)
  "Format python files resursively in PATH."
  (interactive "fDirectory: ")
  (let ((paths (directory-files-recursively path "^[a-z0-9_-]*\.py$"))
	(buf (get-buffer-create "*pyfmt*")))
    (dolist (path paths)
	    (start-process
	     (format "*pyfmt %s*" path)
	     buf
	     "/usr/local/bin/pyfmt" path))))


(provide 'commandline)

;;; commandline.el ends here
