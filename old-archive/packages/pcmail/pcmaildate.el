;;;; GNU-EMACS PCMAIL mail reader

;;  Written by Mark L. Lambert
;;
;;  Internet: markl@us.oracle.com 
;;  USMail:   Oracle Corporation
;; 	      500 Oracle Parkway, box 659410
;;	      Redwood Shores CA 94065
;;  voice:    (415) 506 2912
;;  FAX:      (415) 506 7226

;; Copyright (C) 1989, 1993 Mark L. Lambert

;; This file is not officially part of GNU Emacs, but is being
;; donated to the Free Software Foundation.  As such, it is
;; subject to the standard GNU-Emacs General Public License,
;; referred to below.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;;; global variables

(defconst pcmail-month-alist
  '(("???" 0) ("January" 0) ("February" 31) ("March" 49) ("April" 80) 
    ("May" 110) ("June" 141) ("July" 171) ("August" 202) ("September" 233) 
    ("October" 263) ("November" 294) ("December" 324))
  "Assoc list of month names to number of days since beginning of year.")

;;; date-hacking routines

(defun pcmail-date-less-than-p (a b)
  "Args: (a b)
Return T is message A's date is chronologically before B's, NIL else."
  (< (pcmail-date-triple-to-ndays (pcmail-message-date a))
     (pcmail-date-triple-to-ndays (pcmail-message-date b))))

(defun pcmail-message-date (n)
  "Return specified message's Date: field contents as a date triple.
Args: (n)
  First search the pcmail-date-vector cache for a date triple.  If none is
found, get message N's Date: field and bash it into a triple of the 
form (day month year).  If no date exists, return the triple '(0 0 0)."
  (or (aref pcmail-date-vector n)
      (aset pcmail-date-vector n
	    (cond ((zerop n)
		   '(0 0 0))
		  (t
		    (save-excursion
		      (save-restriction
			(let ((case-fold-search t))
			  (pcmail-narrow-to-unpruned-header n)
			  (or (pcmail-string-to-date-triple 
				(mail-fetch-field "date" nil))
			      '(0 0 0))))))))))

(defun pcmail-month-string-to-num (s)
  "Convert a month name to its number.
Args: (s)"
  (let ((found) (i 0))
    (setq s (downcase (substring s 0 3)))
    (mapcar '(lambda (mon) 
	       (and (string= s (downcase (substring (car mon) 0 3)))
		    (setq found i))
	       (setq i (1+ i)))
	    pcmail-month-alist)
    found))

(defun pcmail-num-to-month-string (n &optional fullname)
  "Convert a month number to its name.  Return NIL if number is not 1-12.
Args: (n)"
  (cond ((< n (length pcmail-month-alist))
	 (if fullname
	     (nth 0 (nth n pcmail-month-alist))
	   (substring (nth 0 (nth n pcmail-month-alist)) 0 3)))))

(defun pcmail-date-triple-to-ndays (date)
  "Turn a date triple into an absolute number.
Args: (date)
  Convert triple DATE (DAY MONTH YEAR) into a number of days by adding DAY
to number of days in year as of beginning of MONTH and number of days in 
year times YEAR.  Amount is not absolutely accurate, but good enough for
our purposes."
  (+ (* 365 (nth 2 date))
     (nth 1 (nth (nth 1 date) pcmail-month-alist))
     (nth 0 date)))

(defun pcmail-date-triple-to-string (date)
  "Format a date triple as a string.
Args: (date)
  Convert triple DATE (day month year) into a string whose format is
determined by the config variable pcmail-date-format. "
  (pcmail-format-string 
   pcmail-date-format
   (list (list "d" '(lambda (date) (nth 0 date)) date)
	 (list "n" '(lambda (date) (nth 1 date)) date)
	 (list "m" '(lambda (date) (pcmail-num-to-month-string (nth 1 date)))
	       date)
	 (list "M" '(lambda (date) 
		      (pcmail-num-to-month-string (nth 1 date) t)) date)
	 (list "y" '(lambda (date) (nth 2 date)) date)
	 (list "Y" '(lambda (date) (+ 1900 (nth 2 date))) date))))

(defun pcmail-string-to-date-triple (&optional s)
  "Convert a date string into a date triple.
Args: (&optional s)
  Convert message date: field string S to a date triple (day month year).
If conversion cannot be performed, return NIL.  If S is NIL, convert today's 
date."
  (let ((day)
	(month)
	(year))
    (or s (setq s (pcmail-todays-date)))
    (cond ((string-match "\\([0-3]?[0-9]\\)[_ \t-]+\\([a-zA-Z][a-zA-Z][a-zA-Z]\\)[_a-z \t-]+\\([0-9][0-9][0-9]*\\)"
			 s)
	   (setq day
		 (string-to-int
		   (substring s (match-beginning 1) (match-end 1)))
		 month
		 (pcmail-month-string-to-num
		  (substring s (match-beginning 2) (match-end 2)))
		 year
		 (string-to-int
		   (substring s (match-beginning 3) (match-end 3))))))
    (cond ((and year day month (not (zerop day)) (> year 0))
	   (and (>= year 1900)
		(setq year (- year 1900)))
	   (list day month year)))))

(defun pcmail-todays-date ()
  "Convert today's date into an RFC822 date.
Args: none"
  (let ((d (current-time-string)))
    (and (string-match (concat "\\([a-zA-Z]+\\)[ ]+\\([a-zA-Z]+\\)[ ]+"
			       "\\([0-9]+\\)[ ]+"
			       "\\([0-9]+:[0-9]+:[0-9]+\\)[ ]+"
			       "19\\([0-9][0-9]\\)")
		       d)
	 (concat (substring d (match-beginning 1) (match-end 1))
		 ", " (substring d (match-beginning 3) (match-end 3))
		 " " (substring d (match-beginning 2) (match-end 2))
		 " " (substring d (match-beginning 5) (match-end 5))
		 " " (substring d (match-beginning 4) (match-end 4))
		 " " pcmail-time-zone))))

(provide 'pcmaildate)
