;;; hk2dn.el --- Convert Harvard-Kyoto to Devanagari  -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;;
;; This package provides a function to convert text in the Harvard-Kyoto (HK) transliteration scheme
;; to Devanagari script.  Requires self-developed command line tool `hk2dn'.
;;
;; To use, select a region of text in Harvard-Kyoto and run:
;; M-x hk-to-devanagari-region

;;; Code:

(defgroup hk2dn nil
  "Customization group for `hk2dn'."
  :group 'editing
  :prefix "hk2dn-")

(defcustom hk2dn-cli-executable "hk2dn"
  "Path to the command line tool `hk2dn'.
If it can be found in system path, \"hk2dn\" is enough.  Otherwise, use
the full path."
  :type 'string
  :group 'hk2dn)

;; --- Interactive Function ---

;;;###autoload
(defun hk-to-devanagari-region (beg end)
  "Convert the text between BEG and END from Harvard-Kyoto to Devanagari.
The conversion logic follows standard Sanskrit transliteration rules."
  (interactive "r")
  (call-process-region beg end hk2dn-cli-executable t t t))

;;;###autoload
(defun hk-to-devanagari-word-at-point ()
  "Convert the symbol at point from Harvard-Kyoto to Devanagari."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (if (not bounds)
        (message "No symbol at point to be transliterated.")
      (hk-to-devanagari-region (car bounds) (cdr bounds)))))


(provide 'hk2dn)

;;; hk2dn.el ends here
