;;;  diary-private.el -- functions for private diaries
;;
;; $Id: diary-private.el,v 1.8 1999/12/27 16:28:51 saschal Exp $

;;; Description:

;;  This file extends diary-lib to maintain a collection of private
;;  diary files, one for a month.  Filenames are choosen an a monthly
;;  basis, e.g. 1999-07 for July 1999 and are stored in the
;;  customizable location specified in `diary-private-dir' (defaults
;;  to "~/.diary").  You can insert default, islamic and hebrew dated
;;  entries.
;;
;;  Defined keystrokes:
;;
;;   i p    - insert-private-diary-entry
;;   i i p  - insert-private-islamic-diary-entry
;;   i h p  - insert-private-hebrew-diary-entry
;;
;;  I'm sorry, but neither islamic nor hebrew entries are marked or
;;  can be viewed from the calendar.  I didn't figure out how to add
;;  this.  Maybe you have some suggestions ;)
;;
;;  Some hooks are used from here:
;;
;;    (add-hook 'diary-display-hook 'fancy-diary-display)
;;    (add-hook 'list-diary-entries-hook 'include-private-diary-files)
;;    (add-hook 'mark-diary-entries-hook 'mark-private-diary-files)
;;
;;  to mark and display your private entries in calendar.  Fancy display is
;;  used since simple display only scans the `main'-diary file.
;;
;;

;;; Installation:

;;  To use this file, just specify
;;
;;                  (require 'diary-private)
;;
;;  in you .emacs file.

;;; Author:

;;  Sascha Luedecke
;;  e-mail: sascha@meta-x.de
;;  www   : http://www.meta-x.de

;;; Code:

(require 'diary-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Adjust this for your needs:

(defcustom diary-private-dir
  (concat (getenv "HOME") "/.diary")
  "Directory containing private diaries, one file per month looking
like 1999-07 for july 1999."
  :group 'diary
  :type 'directory
  :require 'diary-lib
  )

;;; other setting, like hooks and keys
;;
;; Let calendar mark and display all entries
(add-hook 'list-diary-entries-hook 'include-private-diary-files)
(add-hook 'mark-diary-entries-hook 'mark-private-diary-files)
(add-hook 'diary-display-hook 'fancy-diary-display)

(define-key calendar-mode-map "ip"  'insert-private-diary-entry)
(define-key calendar-mode-map "iip"  'insert-private-islamic-diary-entry)
(define-key calendar-mode-map "ihp"  'insert-private-hebrew-diary-entry)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Internal functions used from inside this file
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun diary-private-get-actual-file ()
  "Returns an absolute filename build from current year and month."
  (interactive)
  (let* ((today (calendar-cursor-to-date t))
         (month (extract-calendar-month today))
         (year (extract-calendar-year today))
	 )
    (format "%s/%d-%02d" diary-private-dir year month)
    )
  )


(defun diary-get-private-files ()
  "Returns a list of files matching the three months displayed in
calendar.  All names returned are absolute and reflect real files."
  (let (regexp)
    (save-excursion
      (set-buffer calendar-buffer)
      (setq regexp (format "^%d-\\(%02d\\|%02d\\|%02d\\)$"
			   displayed-year
			   (1- displayed-month)
			   displayed-month
			   (1+ displayed-month))
	    )
      )
    (directory-files diary-private-dir 
		     t
		     regexp
		     )
    )
  )


(defun diary-private-make-entry (arg datefunc)
  "Makes a private diary-entry, computing date string with given
`datefunc', which must be a function, returning string.  If the
choosen date is the current date, a timestamp is added."
  (let ((calendar-date-display-form calendar-date-display-form))
    (if (equal (calendar-cursor-to-date t) (calendar-current-date))
	(setq calendar-date-display-form
	      (append calendar-date-display-form
		      '("  "
			(format-time-string "%X")))
	      )
      )
    ;; No daynames from calendar-date-string
    (if (equal datefunc 'calendar-date-string)
	(setq datefunc (list datefunc '(calendar-cursor-to-date t) nil t))
      (setq datefunc (list datefunc '(calendar-cursor-to-date t)))
      )
    (make-diary-entry (eval-expression datefunc)
		      arg
		      (diary-private-get-actual-file)
		      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions to be used from outside.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun insert-private-diary-entry (arg)
  "Insert an entry into a private diary for the date indicated by point.
Prefix arg will make the entry nonmarking."
  (interactive "P")
  (diary-private-make-entry arg 'calendar-date-string)
  )


(defun insert-private-islamic-diary-entry (arg)
  "Insert an entry into a private diary for the date indicated by point.
Prefix arg will make the entry nonmarking."
  (interactive "P")
  (diary-private-make-entry arg 'calendar-islamic-date-string)
  )


(defun insert-private-hebrew-diary-entry (arg)
  "Insert an entry into a private diary for the date indicated by point.
Prefix arg will make the entry nonmarking."
  (interactive "P")
  (diary-private-make-entry arg 'calendar-hebrew-date-string)
  )


(defun include-private-diary-files ()
  "Includes all private diary files to display them.  This function is
suitable for use in `list-diary-entries-hook' just like
`include-other-diary-files'.  In fact some code was taken from there."
  (let ((private-files (diary-get-private-files))
	diary-file
	(list-diary-entries-hook 'nil)
	)
    (while private-files
      (setq diary-file (car private-files))
      (setq private-files (cdr private-files))

      (unwind-protect
	  (setq diary-entries-list
		(append diary-entries-list
			(list-diary-entries original-date number)))
	(kill-buffer (find-buffer-visiting diary-file))
	)
      )
    )
  )


(defun mark-private-diary-files ()
  "Marks all private diary files.  This function is suitable for use
in `mark-diary-entries-hook' just like `mark-other-diary-files'.  In
fact some code was taken from there."
  (let ((private-files (diary-get-private-files))
	diary-file
	(mark-diary-entries-hook 'nil)
	)
    (while private-files
      (setq diary-file (car private-files))
      (setq private-files (cdr private-files))
      (mark-diary-entries)
      (kill-buffer (find-buffer-visiting diary-file))
      )
    )
  )



(provide 'diary-private)