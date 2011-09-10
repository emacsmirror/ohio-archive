;;;From: howie@cucca.UUCP (Howie Kaye)
;;;Subject: sort library for Gnuemacs V17
;;;Date: 15 Jan 86 20:47:56 GMT

;;;I recently wrote a sort routine for GNUemacs, similar to the ^P
;;;command in TECO emacs.  The function takes three sexprs, and evals
;;;them, to find the start/end of both records and keys.  The records can
;;;be overlapping.  A fourth argument tells whether the sort should be in
;;;ascending, or descending order.  If the "end-of-key" sexpr returns a
;;;numeric value, then that is uses as the key, rather than the string
;;;between the start of key, and the point after eval'ing the end-of-key
;;;expression.  Thus, numeric sorts can be done using it.  The main
;;;routine is called Psort, and it uses the built in "sort" routine,
;;;supplied in V17.  Also included in the library are several simple
;;;sort-commands, ritten using this...ie, sort-lines, sort-paragraphs.
;;;There is also a "sort-by-column-numeric" command, which will do a
;;;numeric sort on a column of text.  Please send any bug reports or
;;;comments to howie@cucca.uucp.

;;;-------sort-library begins here----------------

(defun Psort (startkey-exp endkey-exp nextrec-exp &optional order) 
  "general text sorting routine, similar to ^P in TECO.
Accepts three lisp expressions, and executes them to get to the 
start of a key, the end of a key, and the next record respectively.  
if the endkey expression returns an integer, this is used as the key.
Sort regions are defined as:  the current location through the location
after executing next-key -1.  If order is negative, then the sort is done
in descending order."

  (interactive "xstartkey:
xendkey:
xnextrec:
p")					;get args...three list expressions
  (let ((sort-lists nil)
	(numeric-sort '(nil))
        (start (point))
	(stop (point-max))
	(text (buffer-string)))
					;get all values of start/end-key,
    					; start-region into a list
    (save-excursion
      (setq sort-lists
	    (build-sort-lists startkey-exp endkey-exp nextrec-exp text
			      numeric-sort))
      (reorder-buffer
       (if (car numeric-sort)
	   (if (> order 0)
	       (sort sort-lists 'compare-sort-num-rec-asc)
	     (sort sort-lists 'compare-sort-num-rec-desc))
	 (if (> order 0)
	     (sort sort-lists 'compare-sort-str-rec-asc)
	   (sort sort-lists 'compare-sort-str-rec-desc))) 
       text start stop))))

(defun build-sort-lists(startkey-exp endkey-exp nextrec-exp text numeric-sort) 
  "Build lists to be sorted by sort(), for use with Psort, by eval'ing
each of the sexprs passed to it.  Returns (t) in numeric-sort if the
sort should be numeric (end-key-exp returns an int), and (nil) otherwise."
  (let ((sort-lists nil)
	(start-rec nil)
	(start-key nil)
	(end-key nil)
	(ns nil)
	(x nil))
    (rplaca numeric-sort nil)
    (if (not (eobp))
	(progn
	  (setq start-rec (point))	;save record start
	  (if startkey-exp (eval startkey-exp))	;get to start of key
	  (setq start-key (point))	;save it
	  (if endkey-exp (setq x (eval endkey-exp))) ;get to end of key
	  (if (integerp x)		;if a numeric return
	      (progn
		(setq end-key x)	;use it as the key
		(rplaca numeric-sort t)
		(setq ns t))
	    (setq end-key (point))) ;save it
	  (if nextrec-exp (eval nextrec-exp)) ;get to next record
	  (setq sort-lists (list (list
		  (if ns end-key (substring text (- start-key (point-min))
					    (- end-key (point-min))))
		  start-rec (point))))))
				
    (while (not (eobp))			;until we reach the end of the buffer
      (setq start-rec (point))		;save record start
      (if startkey-exp (eval startkey-exp)) ;get to start of key
      (setq start-key (point))		;save it
      (if endkey-exp (setq x (eval endkey-exp))) ;get to end of key
      (setq end-key (if ns x (point)))	;if a numeric return value...
      (if nextrec-exp (eval nextrec-exp)) ;get to next record
      (setq sort-lists (cons (list	;stick the record in the list
        (if ns end-key (substring text (- start-key (point-min))
				       (- end-key (point-min))))
	       start-rec (point)) sort-lists)))
    (reverse sort-lists)))		;make sort stable

(defun compare-sort-str-rec-asc (rec1 rec2)
  "compare rec1 < rec2 from the sort lists.  A record should look like:
	(key start-rec end-rec)"
  (or (string-lessp (car rec1) (car rec2)) (string= (car rec1) (car rec2))))

(defun compare-sort-str-rec-desc (rec1 rec2)
  "compare rec1 >= rec2 from the sort lists.  A record should look like:
	(key start-rec end-rec)"
  (or (string-lessp (car rec2) (car rec1)) (string= (car rec2) (car rec1))))

(defun compare-sort-num-rec-asc (rec1 rec2)
  "compare rec1 < rec2 from the sort lists.  A record should look like:
	(key start-rec end-rec)"
  (<= (car rec1) (car rec2)))

(defun compare-sort-num-rec-desc (rec1 rec2)
  "compare rec1 >= rec2 from the sort lists.  A record should look like:
	(key start-rec end-rec)"
  (<= (car rec2) (car rec1)))

(defun reorder-buffer (sort-lists text start stop)
  "actually move the text in a buffer which has been sorted"
  (let ((no-quit inhibit-quit))
    (delete-region start stop)		;get rid of old version
    (setq inhibit-quit t)		;can't quit here.
    (goto-char start)
    (while sort-lists 
      (let ((current-rec (car sort-lists)))
	(insert (substring text (- (cadr current-rec) (point-min))
			   (- (caddr current-rec) (point-min)))))
	  (setq sort-lists (cdr sort-lists)))
    (setq inhibit-quit no-quit)))
    
(defun sort-lines (arg) 
  "Sort lines.  If arg is negative, sort backwards."
  (interactive "p")
  (Psort nil '(beginning-of-line 2) nil arg))

(defun sort-paragraphs (arg)
  "Sort paragraphs.  If arg is negative, sort backwards."
  (interactive "p")
  (Psort nil '(forward-paragraph) '(beginning-of-line 2) arg))

(defun sort-pages (arg)
  "Sort pages.  If arg is negative, sort backwards."
  (interactive "p")
  (Psort nil '(forward-page) '(beginning-of-line 2) arg))

(defun sort-by-column-numeric (column)
  "Sort a table of numbers by the nth column, numerically.
the specified column must all be numeric, and the table must have all
n columns in every row.  With a negative arg, sorts by the -arg'th column,
backwards."
  (interactive "pcolumn: ")
  (if (= 0 column) (setq column 1))
  (let ((pos 0) (order (if (< column 0) -1 1)))
    (if (< column 0) (setq column (- 0 column)))
    (setq c column)
    (Psort '(progn (if (> column 1)
		       (if (not (forward-column (1- column))) 
			   (error "not enough columns in line"))))
	   '(progn
	      (setq pos (point))
	      (if (not (forward-column 1))
		  (error "not enough columns in line"))
	      (string-to-int (buffer-substring pos (point))))
	   '(beginning-of-line 2)
	   order)))

(defun sort-by-column-string (column)
  "Lexically sort a table by the nth column.  There must be at least n columns
in every row.  With a negative argument, sorts by (abs arg), backwards."
  (interactive "pcolumn: ")
  (if (= 0 column) (setq column 1))
  (let ((pos 0) (order (if (< column 0) -1 1)))
    (if (< column 0) (setq column (- 0 column)))
    (Psort '(progn (if (> column 1)
		       (if (not (forward-column (1- column))) 
			   (error "not enough columns in line"))))
	   '(progn
	      (setq pos (point))
	      (if (not (forward-column 1))
		  (error "not enough columns in line")))
	   '(beginning-of-line 2)
	   order)))

(defun forward-column (n)
  "move forward past n columns of text, where a column is terminated
by whitespace"
  (interactive "pcolumns: ")
  (let ((eol (save-excursion (end-of-line 1) (point))))
    (skip-chars-forward "^a-zA-z0-9,\-_") ;get to beginning of this column
    (if (re-search-forward "[^a-zA-z0-9,\-_]" (1+ eol) 'move n)
	(prog2 
	 (skip-chars-backward "^a-zA-z0-9,\-_")
	 t)
      (prog2
       (beginning-of-line 0)
       nil))))

(defun sort-region (sort-func)
  "sort a region, based on another type of sort"
  (save-excursion 
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (goto-char (point-min))
      (eval sort-func))))

(defun sort-lines-region (arg)
  "Sort lines between point and mark.  with negative arg, sorts backwards."
  (interactive "p")
  (sort-region '(sort-lines arg)))

(defun sort-pages-region (arg)
  "Sort pages between point and mark.  with negative arg, sorts backwards."
  (interactive "p")
  (sort-region '(sort-pages arg)))

(defun sort-paragraphs-region (arg)
  "Sort paragraphs between point and mark.  with negative arg, sorts 
backwards."
  (interactive "p")
  (sort-region '(sort-pages arg)))

(defun sort-by-column-numeric-region (arg)
  "Sort numerically by arg'th column between point and mark.  with negative 
arg, sorts backwards."
  (interactive "p")
  (sort-region '(sort-by-column-numeric arg)))

(defun sort-by-column-string-region (arg)
  "Sort lexically by arg'th column between point and mark.  with negative 
arg, sorts backwards."
  (interactive "p")
  (sort-region '(sort-by-column-string arg)))

;;;-- 
;;;Howie Kaye				Sy.Howie@CU20B.ARPA             
;;;Columbia University 			HKAUS@cuvma (bitnet)          
;;;System's Integration Group		{?}!seismo!columbia!cucca!howie
