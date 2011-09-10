;;; jhg-pine.el --- Read a pine addressbook
;;
;; ~/lib/emacs/jhg-lisp/jhg-pine.el ---
;;
;; $Id: jhg-pine.el,v 1.2 1999/08/18 00:30:49 harley Exp $
;;

;; Author:  James H Gorrell <harley@bcm.tmc.edu>
;; Keywords:

;;; Commentary:
;; * Read a pine address book

;;; Code:
(defvar jhg-pine-alias-loaded nil
  "Have the pine aliases been loaded?")

;;;###autoload
(defun jhg-pine-alias-maybe-load ()
  "Load the pine aliases if not done already."
  (if (not jhg-pine-alias-loaded)
      (progn
	(jhg-pine-alias-read-book "~/.pine.addressbook")
	(setq jhg-pine-alias-loaded t)
	)))

;;;###autoload
(defun jhg-pine-alias-read-book (file)
  "Read the FILE as a pine address book."
  (interactive "fPine Addressbook:")
  (let ((buff (get-buffer-create " *pine-addresses*")))
    (save-excursion
      (set-buffer buff)
      (erase-buffer)
      (insert-file-contents file nil)
      (jhg-pine-alias-process-buffer)
      (kill-buffer buff)
      )))

(defun jhg-pine-alias-process-buffer ()
  "Read the pine aliases out of the current buffer."
  (beginning-of-buffer)
  (let (alias address)
    ;; Grab what we can; ignore the rest...
    (while (search-forward-regexp
	    "^\\([A-Za-z]+\\)\t[^\t]*\t(?\\([^\t\n)]*\\))?"
	    (point-max) t)
      (setq alias   (match-string-no-properties 1)
	    address (match-string-no-properties 2))
      ;(message "%s %s" alias address)
      (define-mail-abbrev alias address)
      )))

;(jhg-pine-alias-read-book "~/.pine.addressbook")

(provide 'jhg-pine)
;;; jhg-pine.el ends here
