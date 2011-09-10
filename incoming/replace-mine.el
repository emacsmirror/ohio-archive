; -*- Mode: Emacs-Lisp; -*- 

;  Eric Beuscher
;  Tulane University
;  Department of Computer Science
;  replace-mine.el

;;; file for adding my procedural-query-replace and
;;; procedural-query-replace-regexp.    a way to make the replace a procedure
;;; and not a string


(defvar procedural-query-replace-type 'on-replace
  "Determine which internal query-replace is called

possible values:
'on-replace	only call replace-proc when a match is found and user confirms
		the replace
'on-iteration	call replace-proc after each match, regardless of whether it is
		to be replaced or not
(not 'on-iteration) = 'on-match
")

;;; *************** PROC QUERY-REPLACE ***************

(defun procedural-query-replace-internal-on-iteration
  (search-proc search replace-proc)
  (while (funcall search-proc search nil t)
    (let* ((start (match-beginning 0))
	   (end (match-end 0))
	   (match (buffer-substring-no-properties start end))
	   (replace (funcall replace-proc match))
	   )
      (unwind-protect
	  (progn
	    (replace-highlight start end)
	    (if (y-or-n-p (format "Replace %S with %S ? " match replace))
		(progn (delete-region start end)
		       (goto-char start)
		       (insert replace)
		       )))
	(replace-dehighlight))
      )))

(defun procedural-query-replace-internal-on-match
  (search-proc search replace-proc)
  (while (funcall search-proc search nil t)
    (let* ((start (match-beginning 0))
	   (end (match-end 0))
	   (match (buffer-substring-no-properties start end))
	   )
      (unwind-protect
	  (progn
	    (replace-highlight start end)
	    (if (y-or-n-p (format "Replace %S? " match))
		(progn (delete-region start end)
		       (goto-char start)
		       (insert (funcall replace-proc match))
		       )))
	(replace-dehighlight))
      )))

(defun procedural-query-replace-read-args (string)
  (let (from to)
     (setq from (read-from-minibuffer (format "Procedural query replace %s: "
					      string)
				      nil nil nil
				      'query-replace-history))
     (setq to (read-from-minibuffer (format "replace %s %s using procedure: "
					    string from)
			       nil nil
			       t
			       'query-replace-history))
     (list from to)))

(defun procedural-query-replace (search replace-proc)
  "Do a query replace where the replace is the result of a procedure call.
Given a search string, and a procedure of one-argument,
when matches of search are found, run replace-proc with the match as its one
argrument.
I believe this allows me to do more interesting search and replace items.

What follows are three examples, one i ran interactively, and the last two
non-interactively (the 2nd could be interactive, but was written before i added
interactivity). (Escapes have been added to double-quotes that appear in the
examples)

;; look for one of two strings, when found, place a period in front of the
;; matched string
(procedural-query-replace-regexp
 \"\\(hos\\|peo\\)\"
 (quote (lambda (match)
	  (concat \".\" match))))

;; look for one of two strings, when found, chop off only the first character
(procedural-query-replace-regexp
 \"\\(\\.host\\|\\.people\\)\"
 (function (lambda (match)
	     (substring match 1 (length match)))))

;; search for a string with numeric data, something that was maybe cut and
;; multiply pasted.  use a static variable to place an incremented number on
;; each subsequent match
(let ((count -1))
  (procedural-query-replace
   \"[0]\" ; search
   (function (lambda (match) ; replace-proc
	       (setq count (+ count 1))
	       (concat \"[\" count \"]\")))))
"
  (interactive (procedural-query-replace-read-args ""))
  (let ((qr (cond ((eq 'on-iteration procedural-query-replace-type)
		   (function procedural-query-replace-internal-on-iteration))
		  (t
		   (function procedural-query-replace-internal-on-match)))))
    (funcall qr (function search-forward) search replace-proc)))

(defun procedural-query-replace-regexp (search replace-proc)
  "Do a query replace where the replace is the result of a procedure call.
Given a search string, and a procedure of one-argument,
when matches of search are found, run replace-proc with the match as its one
argrument.
I believe this allows me to do more interesting search and replace items.

What follows are three examples, one i ran interactively, and the last two
non-interactively (the 2nd could be interactive, but was written before i added
interactivity). (Escapes have been added to double-quotes that appear in the
examples)

;; look for one of two strings, when found, place a period in front of the
;; matched string
(procedural-query-replace-regexp
 \"\\(hos\\|peo\\)\"
 (quote (lambda (match)
	  (concat \".\" match))))

;; look for one of two strings, when found, chop off only the first character
(procedural-query-replace-regexp
 \"\\(\\.host\\|\\.people\\)\"
 (function (lambda (match)
	     (substring match 1 (length match)))))

;; search for a string with numeric data, something that was maybe cut and
;; multiply pasted.  use a static variable to place an incremented number on
;; each subsequent match
(let ((count -1))
  (procedural-query-replace
   \"[0]\" ; search
   (function (lambda (match) ; replace-proc
	       (setq count (+ count 1))
	       (concat \"[\" count \"]\")))))
"
  (interactive (procedural-query-replace-read-args "regexp"))
  (let ((qr (cond ((eq 'on-iteration procedural-query-replace-type)
		   (function procedural-query-replace-internal-on-iteration))
		  (t
		   (function procedural-query-replace-internal-on-match)))))
    (funcall qr (function re-search-forward)
	     search
	     replace-proc)))

;; look for one of two strings, when found, place a period in front of the
;; matched string
;;;(procedural-query-replace-regexp
;;; "\\(hos\\|peo\\)"
;;; (quote (lambda (match)
;;;	  (concat "." match))))

;; look for one of two strings, when found, chop off only the first character
;;;(procedural-query-replace-regexp
;;; "\\(\\.host\\|\\.people\\)"
;;; (function (lambda (match)
;;;	     (substring match 1 (length match)))))

;; search for a string with numeric data, something that was maybe cut and
;; multiply pasted.  use a static variable to place an incremented number on
;; each subsequent match
;;;(let ((count -1))
;;;  (procedural-query-replace
;;;   "[0]" ; search
;;;   (function (lambda (match) ; replace-proc
;;;	       (setq count (+ count 1))
;;;	       (concat "[" count "]")))))

(provide 'replace-mine)