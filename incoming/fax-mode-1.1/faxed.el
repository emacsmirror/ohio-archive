;;; faxed.el -- a fax specific minor mode for Dired.
;;;
;;; Copyright (C) 1995 Ralph Schleicher
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2 of
;;; the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;; This file is not part of GNU Emacs.
;;;
;;; Author: Ralph Schleicher <rs@purple.IN-Ulm.DE>
;;; Maintainer: see the `Author' field
;;; Keywords: local comm fax
;;; Comments: Add the lines
;;;
;;;	(autoload 'faxed-mode "faxed"
;;;	  "Faxed mode is a minor mode for Dired." t)
;;;
;;;	(autoload 'faxed "faxed"
;;;	  "Run Dired on directory DIRECTORY with turned on Faxed mode." t)
;;;
;;; to your `site-start.el' file.
;;; This mode have still to be improved.
;;; Time-stamp: "Wed Dec 27 18:38:14 MET 1995 rs@purple.IN-Ulm.DE"
;;; Code:


(require 'faxutil)


(defvar faxed-mode nil
  "Non-`nil' enables Faxed mode.")
(make-variable-buffer-local 'faxed-mode)

(defvar faxed-mode-string " Fax"
  "Indicator included in the mode line when in Faxed mode.")

(or (assq 'faxed-mode minor-mode-alist)
    (setq minor-mode-alist (append minor-mode-alist
				   (list '(faxed-mode faxed-mode-string)))))

(defvar faxed-mode-map (make-sparse-keymap)
  "Minor mode map for Faxed mode.")

(or (assq 'faxed-mode minor-mode-map-alist)
    (setq minor-mode-map-alist (cons (cons 'faxed-mode faxed-mode-map)
				     minor-mode-map-alist)))

;;;### autoload
(defun faxed-mode (&optional arg)
  "Faxed mode is a minor mode for Dired.
It redefines some keys so that the user can, for example, view a fax message
by simply pressing `v'.
Optional prefix argument unconditionally turns on Faxed mode if the argument
is greater than zero, otherwise turn Faxed mode off.  No prefix argument at
all toggles Faxed mode."
  (interactive "P")
  (setq faxed-mode (if (null arg)
		       (not faxed-mode)
		     (> (prefix-numeric-value arg) 0)))
  (set-buffer-modified-p (buffer-modified-p)))

(define-key faxed-mode-map "v" 'faxed-view-file)
(define-key faxed-mode-map "V" 'faxed-view-marked-files)
(define-key faxed-mode-map "\M-v" 'faxed-view-pages)
(define-key faxed-mode-map "w" 'faxed-print-file)
(define-key faxed-mode-map "W" 'faxed-print-marked-files)
(define-key faxed-mode-map "\M-w" 'faxed-print-pages)

(define-key faxed-mode-map [menu-bar faxed]
  (cons "Faxed" (make-sparse-keymap "Faxed")))
(define-key faxed-mode-map [menu-bar faxed print-pages]
  '("Print Pages" . faxed-print-pages))
(define-key faxed-mode-map [menu-bar faxed print-marked-files]
  '("Print Marked Files" . faxed-print-marked-files))
(define-key faxed-mode-map [menu-bar faxed print-file]
  '("Print File" . faxed-print-file))
(define-key faxed-mode-map [menu-bar faxed view-pages]
  '("View Pages" . faxed-view-pages))
(define-key faxed-mode-map [menu-bar faxed view-marked-files]
  '("View Marked Files" . faxed-view-marked-files))
(define-key faxed-mode-map [menu-bar faxed view-file]
  '("View File" . faxed-view-file))

(defvar faxed-page-extension-regexp "\\.[^.]+"
  "*Regular expression matching the file name extension of a fax page.
Some fax receiving programs save each fax page in a single file.  This
regular expression will be added to the basename of a file name to find
all pages of a fax message.")

(defun faxed-get-pages (&optional localp)
  "Return all file names with the same basename as the current file.
Optional argument LOCALP with value `no-dir' means don't include the
directory name in the result.  A non-`nil' value means return names
relative to `default-directory'."
  (let ((regexp (concat (regexp-quote (file-name-sans-extension
				       (dired-get-filename 'no-dir)))
			faxed-page-extension-regexp "$"))
	(result '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
	(setq result (cons (dired-get-filename 'no-dir) result))))
    (if (eq localp 'no-dir)
	(sort result 'string-lessp)
      (let ((dir (dired-current-directory localp)))
	(mapcar (function (lambda (name)
			    (concat dir name)))
		(sort result 'string-lessp))))))

(defun faxed-view-file ()
  "View the current file."
  (interactive) (fax-view-file (dired-get-filename)))

(defun faxed-view-marked-files ()
  "View the marked files."
  (interactive) (fax-view-file (dired-get-marked-files)))

(defun faxed-view-pages ()
  "View all files with the same basename as the current file."
  (interactive) (fax-view-file (faxed-get-pages)))

(defun faxed-print-file ()
  "Print the current file."
  (interactive) (fax-print-file (dired-get-filename)))

(defun faxed-print-marked-files ()
  "Print the marked files."
  (interactive) (fax-print-file (dired-get-marked-files)))

(defun faxed-print-pages ()
  "Print all files with the same basename as the current file."
  (interactive) (fax-print-file (faxed-get-pages)))


(defvar faxed-default-directory
  (concat "/var/spool/fax/in.coming/" user-login-name)
  "*The default directory to be inserted when calling `faxed'.
`faxed-default-directory' should point to the user's fax incoming directory.")

(defvar faxed-listing-switches "-Altr"
  "*Switches passed to `ls' for Dired.")

(defvar faxed-hook nil)

;;;### autoload
(defun faxed (directory)
  "Run Dired on directory DIRECTORY with turned on Faxed mode."
  (interactive
   (list (read-file-name "Faxed directory: "
			 faxed-default-directory
			 faxed-default-directory t)))
  (or (file-directory-p directory)
      (error "%s: No such directory"))
  (dired directory faxed-listing-switches)
  (run-hooks 'faxed-hook)
  (faxed-mode 1))


(provide 'faxed)


;;; faxed.el ends here
