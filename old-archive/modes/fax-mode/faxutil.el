;;; faxutil.el -- basic fax functions.
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
;;; Comments: Add
;;;
;;;	(autoload 'fax-read-resource-file "faxutil"
;;;	  "Read a fax resource file." t)
;;;
;;;	(autoload 'fax-define-fax-alias "faxutil"
;;;	  "Define a fax alias." t)
;;;
;;;	(autoload 'fax-number "faxutil"
;;;	  "Insert the fully resolved phone number of a fax alias at point." t)
;;;
;;;	(autoload 'fax-simple-number "faxutil"
;;;	  "Insert the phone number of a fax alias at point." t)
;;;
;;;	(autoload 'fax-file-name "faxutil"
;;;	  "Read a file name from the mini-buffer and insert it at point." t)
;;;
;;;	(autoload 'fax-view-file "faxutil"
;;;	  "View a facsimile message." t)
;;;
;;;	(autoload 'fax-print-file "faxutil"
;;;	  "Print a facsimile message." t)
;;;
;;; to the `site-start.el' file so that the user can define fax aliases
;;; interactively or in `.emacs'.
;;; Time-stamp: "Thu Dec 21 09:52:08 MET 1995 rs@purple.IN-Ulm.DE"
;;; Code:


(require 'phone)


(defvar fax-resource-file-name "~/.faxrc"
  "*Name of file for setting resources like fax aliases.
See the documentation of the `fax-read-resource-file' function for a
description of the file format.")

(defvar fax-resource-file-modification-time nil
  "The modification time of the `fax-resource-file-name' file when it
was last examined.")

(defvar fax-resource-file-modified t
  "Non-`nil' if the `fax-resource-file-name' file have to be read again.")

(defun fax-synchronize-resources ()
  (let ((mod-time (nth 5 (file-attributes fax-resource-file-name))))
    (or (equal fax-resource-file-modification-time mod-time)
	(setq fax-resource-file-modification-time mod-time
	      fax-resource-file-modified t))
    (and (file-exists-p fax-resource-file-name)
	 (eq fax-resource-file-modified t)
	 (progn
	   (setq fax-resource-file-modified nil)
	   (fax-read-resource-file)))))

;;;### autoload
(defun fax-read-resource-file (&optional file-name)
  "Read a fax resource file.
FILE-NAME defaults to `fax-resource-file-name' (which points normally to
`~/.faxrc').  Only fax aliases of the form

    alias NAME VALUE

will be evaluated -- the other file contents is ignored.  The alias keyword
defines NAME as an alias for VALUE.  VALUE can be either the final phone
number or another alias.  Multiple numbers have to be separated by commas.
VALUE will be treated as a shell command if the first character of VALUE is
a vertical bar.  Newlines in the shell command output will be substituted
with commas."
  (interactive "fFax resource file: ")
  (setq file-name (expand-file-name (or file-name fax-resource-file-name)))
  (let ((orig-buf (current-buffer))
	(temp-buf nil))
    (unwind-protect
	(let ((case-fold-search nil))
	  (message "Parsing %s..." file-name)
	  (setq temp-buf (generate-new-buffer " faxrc"))
	  (buffer-disable-undo temp-buf)
	  (set-buffer temp-buf)
	  (insert-file-contents file-name)
	  (goto-char (point-max))
	  (or (eq (preceding-char) ?\n)
	      (insert ?\n))
	  (goto-char (point-min))
	  (while (re-search-forward "^[ \t]*alias[ \t]+" nil t)
	    (re-search-forward "[^ \t\n]+")
	    (let* ((name (buffer-substring (match-beginning 0) (match-end 0)))
		   (start (progn
			    (skip-chars-forward " \t") (point)))
		   (value (progn
			    (end-of-line) (buffer-substring start (point)))))
	      (fax-define-fax-alias name value)))
	  (message "Parsing %s... done" file-name))
      (if temp-buf
	  (kill-buffer temp-buf))
      (set-buffer orig-buf))))


(defvar fax-aliases nil
  "*Alist of phone number aliases,
List members are cons cells of the form

    (NAME . VALUE)

where NAME is the fax alias for VALUE.")

;;;### autoload
(defun fax-define-fax-alias (name value)
  "Define NAME as a fax alias for VALUE.
VALUE can be either the final phone number or another alias.  Multiple
numbers have to be separated by commas.	 VALUE will be executed in an
inferior shell if the first character of VALUE is a vertical bar.  The
output of the shell command replaces the old contents of VALUE (newline
characters are substituted by commas)."
  (interactive "sDefine fax alias: \nsDefine `%s' as a fax alias for: ")
  (if (string-match "\\`[ \t]*|" value)
      (let ((orig-buf (current-buffer))
	    (temp-buf nil))
	(setq value (substring value (match-end 0)))
	(unwind-protect
	    (progn
	      (setq temp-buf (generate-new-buffer " faxsh"))
	      (buffer-disable-undo temp-buf)
	      (set-buffer temp-buf)
	      (if (not (zerop (call-process shell-file-name nil temp-buf nil
					    shell-command-switch value)))
		  (error "Shell command `%s' failed" value))
	      (goto-char (point-min))
	      (while (re-search-forward "^[ \t\n]+" nil t)
		(replace-match ""))
	      (goto-char (point-min))
	      (while (re-search-forward "[ \t]*\n" nil t)
		(replace-match ", "))
	      (goto-char (point-max))
	      (and (= (preceding-char) ? )
		   (backward-delete-char 2))
	      (setq value (buffer-string)))
	  (if temp-buf
	      (kill-buffer temp-buf))
	  (set-buffer orig-buf))))
  (let ((known (assoc name fax-aliases)))
    (if known
	(setcdr known value)
      (setq fax-aliases (cons (cons name value) fax-aliases)))))


(defun fax-object-value (object)
  "Evaluate OBJECT if it is a function, variable or lambda expression;
else do nothing."
  (if (symbolp object)
      (cond ((fboundp object)
	     (funcall object))
	    ((boundp object)
	     (symbol-value object)))
    (if (and (consp object) (eq (car object) 'lambda))
	(funcall object)
      (identity object))))

(defun fax-trim-string (string &optional prefix postfix)
  "Delete PREFIX at the beginning and POSTFIX at the end of STRING.
PREFIX and POSTFIX are regular expressions.  `nil' deletes whitespace,
any non-string is a no-op."
  (if (null prefix)
      (setq prefix "[ \t\n]+"))
  (if (and (stringp prefix) (string-match (concat "\\`" prefix) string))
      (setq string (substring string (match-end 0))))
  (if (null postfix)
      (setq postfix "[ \t\n]+"))
  (if (and (stringp postfix) (string-match (concat postfix "\\'") string))
      (setq string (substring string 0 (match-beginning 0))))
  string)

(defun fax-split-string (string &optional regexp empty)
  "Split STRING separated by REGEXP into a list.
REGEXP defaults to a single comma (whitespace before and after the comma is
ignored).  Non-`nil' EMPTY means keep empty strings in the resulting list."
  (if (null regexp)
      (setq regexp "[ \t\n]*,[ \t\n]*"))
  (let ((result '())
	(start (and (not (equal string "")) 0))
	(len (length string))
	(end nil))
    (while start
      (setq end (string-match regexp string start)
	    result (cons (substring string start end) result)
	    start (and end (/= (match-end 0) len) (match-end 0))))
    (if (null empty)
	(setq result (delete "" result)))
    (nreverse result)))


;;;### autoload
(defun fax-number (alias &optional no-resolve)
  "Insert the fully resolved fax phone number of ALIAS at point.
Multiple numbers will be separated by \", \".  Prefix argument means
do not recursively resolve phone number aliases."
  (interactive
   (progn
     (fax-synchronize-resources)
     (list (completing-read "Fax alias: " fax-aliases)
	   (identity current-prefix-arg))))
  (insert (mapconcat 'identity (fax-number-1 alias (not no-resolve)) ", ")))

;;;### autoload
(defun fax-simple-number (alias &optional resolve)
  "Insert the fax phone number of ALIAS at point.
Multiple numbers will be separated by \", \".  Prefix argument means
recursively expand phone number aliases."
  (interactive
   (progn
     (fax-synchronize-resources)
     (list (completing-read "Fax alias: " fax-aliases)
	   (identity current-prefix-arg))))
  (insert (mapconcat 'identity (fax-number-1 alias resolve) ", ")))

(defun fax-phone-number (alias &optional resolve)
  "Translate ALIAS to a list of phone numbers.
Recursively expand fax aliases if RESOLVE is non-`nil'."
  (mapcar 'phone-number-dial-string (fax-number-1 alias resolve)))

(defun fax-number-1 (alias resolve)
  (let* ((number (cdr (assoc alias fax-aliases)))
	 (result (fax-split-string (or number alias))))
    (if (and resolve result)
	(let (done)
	  (while result
	    (while (setq alias (car result)
			 number (cdr (assoc alias fax-aliases)))
	      (setq result (nconc (fax-split-string number)
				  (cdr result))))
	    (if (not (assoc alias done))
		(setq done (cons (cons alias t) done)))
	    (setq result (cdr result)))
	  (setq result (nreverse (mapcar 'car done)))))
    result))

;;;### autoload
(defun fax-file-name (file-name)
  "Read a file name from the mini-buffer and insert it at point."
  (interactive "fFile name: ")
  (insert (expand-file-name file-name)))

(defun fax-query-phone-numbers (&optional prompt)
  "Read phone numbers from the mini-buffer.
The optional argument PROMPT is the string to display to ask for the phone
number, defaults to \"Send fax to: \"."
  (if (not (stringp prompt))
      (setq prompt "Send fax to: "))
  (let ((result '())
	(number nil))
    (while (progn
	     (setq number (completing-read prompt fax-aliases))
	     (not (string-match number "\\`[ \t,]*\\'")))
      (let ((scratch (fax-phone-number number)))
	(while scratch
	  (setq result (cons (car scratch) result)
		scratch (cdr scratch)))))
    (setq result (fax-phone-number
		  (mapconcat 'identity (nreverse result) ", ") t))))

(defun fax-query-file-names (&optional prompt)
  "Read file names from the mini-buffer.
The optional argument PROMPT is the string to display to ask for the file
name, defaults to \"Attach file: \"."
  (if (not (stringp prompt))
      (setq prompt "Attach file: "))
  (let ((result '())
	(name nil))
    (while (progn
	     (let ((insert-default-directory t))
	       (setq name (read-file-name prompt nil "" t)))
	     (and (not (equal name ""))
		  (file-readable-p name)
		  (file-regular-p name)))
      (setq result (cons (expand-file-name name) result)))
    (nreverse result)))


(defvar fax-view-program nil
  "*Program used to view facsimile messages; defaults to \"viewfax\".")

(defvar fax-view-switches nil
  "*List of extra arguments when `fax-view-program' is invoked.")

;;;### autoload
(defun fax-view-file (files)
  "View FILES by calling `fax-view-program'.
FILES is either a single file name (a string) or a list of file names."
  (interactive "fView file: ")
  (apply 'call-process
	 (nconc (list (or fax-view-program
			  "viewfax")
		      nil 0 nil)
		(append fax-view-switches
			(if (listp files)
			    (mapcar 'expand-file-name files)
			  (list (expand-file-name files)))))))

(defvar fax-print-program nil
  "*Program used to print facsimile messages; defaults to \"printfax\".")

(defvar fax-print-switches nil
  "*List of extra arguments when `fax-print-program' is invoked.")

;;;### autoload
(defun fax-print-file (files)
  "Print FILES by calling `fax-print-program'.
FILES is either a single file name (a string) or a list of file names."
  (interactive "fPrint file: ")
  (apply 'call-process
	 (nconc (list (or fax-print-program
			  "printfax")
		      nil 0 nil)
		(append fax-print-switches
			(if (listp files)
			    (mapcar 'expand-file-name files)
			  (list (expand-file-name files)))))))


(provide 'faxutil)


;;; faxutil.el ends here
