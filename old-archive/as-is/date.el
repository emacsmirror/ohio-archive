;Date: Tue, 27 Dec 88 13:06:14 EST
;From: suneast!symbol!cdr@Sun.COM (Constantine Rasmussen - Sun ECD)
;To: dsill@k30c, arpa-unix-emacs-request@PIZZA.BBN.COM
;Subject: Modification to date routines

;;; -*- Mode: lisp-interaction ; Syntax: elisp -*-
;;;
;;;     File: date.el
;;;
;;;     This is a collection of functions to get and insert the date,
;;;     user name & time.  They are useful to append logs and make notes
;;;     in sources.
;;;
;;; Constantine Rasmussen           Sun Microsystems, East Coast Div.   F658
;;;    (508) 671-0404               2 Federal Street; Billerica, Mass. 01824
;;;  ARPA: cdr@sun.com         USENET: {cbosgd,decvax,hplabs,seismo}!sun!cdr
;;;
;;;     Last Modified: Constantine Rasmussen, Dec 27, 1988
;;;

(provide 'date)


(defconst date-month-number
  '(("Jan" . 1) ("Feb" . 2) ("Mar" . 3) ("Apr" . 4) ("May" . 5) ("Jun" . 6)
    ("Jul" . 7) ("Aug" . 8) ("Sep" . 9) ("Oct" . 10) ("Nov" . 11) ("Dec" . 12))
  "Assoc list for looking up the month number from the month
abbreviation.")


(defun insert-user-datestamp (&optional append-time)
  "Args: (&OPTIONAL APPEND-TIME)
Inserts userid and date stamp.  If APPEND-TIME is non-nil, then the
result will be: \"<USER NAME>, MMM DD, YYYY HH:MM\".  Useful for
timestamping in \"live\" files such as source code or logs."
  (interactive "P")
  (insert (strip-aux-GCOS-info (user-full-name)) ", "
	  (current-date-and-time (not append-time))))

;;; This is, of course, user's choice
(global-set-key "\C-ct" 'insert-user-datestamp)


(defun insert-star-date-string (&optional time-also)
  "Args: (&OPTIONAL TIME-ALSO)
Inserts current stardate at point.  Useful for timestamps that must be
sorted.  If the prefix argument TIME-ALSO is set the time is appended
resulting in \"19481112.0700\".  Similar to the stardate used on Star
Trek."
  (interactive "P")
  (insert (star-date-string time-also)))


(defun strip-aux-GCOS-info (fullname)
  (substring fullname 0 (string-match " *[-:]" fullname)))


(defun current-date-string (&optional prepend-weekday)
  "Args: (&OPTIONAL PREPEND-WEEKDAY)
Returns date string in the format of Nov 2, 1988. if PREPEND-DATE is
non-null, it returns the format Wed. Nov 2, 1988"

  (concat (cond (prepend-weekday
		 (concat (substring (current-time-string) 0 3) ". "))
		(t "")) 
	  (substring (current-time-string) 4 10)
	  ", " (substring (current-time-string) -4 nil)))


;;; date function (useful for logging times in job notes, etc ...)
(defun current-date-and-time (&optional date-only prepend-weekday)
  "Args: (&OPTIONAL DATE-ONLY PREPEND-WEEKDAY)
Returns string with the current date and time.  If DATE-ONLY is
non-nil, time is omitted.  If PREPEND-WEEKDAY is non-nil, the
abbreviated day of the week is added to the front of the string
resulting in \"Tue, Jul 12, 1988, 02:17\"."

  (if date-only
      (current-date-string)
    (concat (current-date-string prepend-weekday) 
	    (substring (current-time-string) 10 16))))


(defun star-date-string (&optional time-also year month day hour minute)
  "Args: (&OPTIONAL TIME-ALSO YEAR MONTH DAY HOUR MINUTE) Returns
string with star-date-string.  Useful for timestamps that must be sorted.  If
TIME-ALSO is non-nil the current time is appended resulting in
\"19481112.0700\".  Any element in the time arguments that is nil will
have the current time information substituted.  Similar to the
stardate used on Star Trek."
  (let (date-str
	(cur-time (current-time-string))) ; "Fri Dec  9 14:24:12 1988"
    (if (not year)
	(setq year (string-to-int (substring cur-time -4 nil))))
    (if (not month)
	(setq month
	      (cdr (assoc (substring cur-time 4 7) date-month-number))))
    (if (not day)
	(setq day (string-to-int (substring cur-time 8 10))))
    (cond (time-also
	   (if (not hour)
	       (setq hour (string-to-int (substring cur-time 11 13))))
	   (if (not minute)
	       (setq minute (string-to-int (substring cur-time 14 16))))))
    (setq date-str
	  (concat
	   (format "%d%02d%02d" year month day)
	   (if time-also
	       (format ".%02d%02d" hour minute))))
    (if (interactive-p)
	(insert date-str))
    date-str))



