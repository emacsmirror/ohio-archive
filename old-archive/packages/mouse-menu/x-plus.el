;;;; X windows fripperies. Essentially just to shock the neighbours.
;;;; Russell Ritchie, Scottish HCI Centre, <russell@uk.ac.strath.hci>.
;;;; Wed Apr 19 16:30:42 1989

(require 'gensym)
(provide 'x-plus)

(defvar nil-synonym (gensym)
  "Synonym for nil when it is desired that this value be explicitly selected.")

(defun x-mouse-query (arg question answers &optional must-select)
  "At window position ARG, ask QUESTION. Return selected item from ANSWERS.
ARG is a list (x-pos y-pos).
QUESTION is a string.
ANSWERS is a list of strings or symbols or lists. If strings or
symbols the selected string or symbol is displayed and returned when
selected. If lists the car (which must be a string) is displayed and
the cdr returned when it is selected.

If optional third arg MUST-SELECT is non-nil one of ANSWERS
must be selected; querying will continue until a selection is made.
N.b. X responds to both mouse-up and mouse-down events equally: it's not
beeping twice, you've just had 2 opportunities to select the right thing.
See also x-mouse-select-item."
  (let* ((menu-items
	  (if (consp (car answers))
	      (if must-select
		  ;; The user may desire some options to return nil, as in the
		  ;; yes-or-no-p example below. Cater for this with a synonym.
		  (mapcar (function (lambda (x)
				      (if (null (cdr x))
					  (cons (car x) nil-synonym)
					x)))
			  answers)
		answers)
	    (mapcar (function (lambda (x) (cons (format "%s" x) x)))
		    answers)))
	 (query-menu (list "Query Menu" (cons question menu-items)))
	 (selection (x-popup-menu arg query-menu)))
    (if must-select
	(if (null selection)
	    (let* ((all-rev (reverse (mapcar 'car menu-items)))
		   (but-last (cdr all-rev))
		   (last (car all-rev))
		   (all-but-last (reverse but-last))
		   (mesg (format
			  "You must select one of: %s or %s."
			  (mapconcat 'identity all-but-last ", ") last)))
	      (while (null selection)
		(message mesg)
		(ding)
		(setq selection (x-popup-menu arg query-menu)))
	      (if (eq selection nil-synonym)
		  nil
		selection))
	  (if (eq selection nil-synonym)
	      nil
	    selection))
      selection)))

(defun x-mouse-yes-or-no-p (question &optional arg)
  "Ask a yes or no question. Force user to explicitly select yes or no.
Optional 2nd arg POS is a list (x y) of x and y coordinates for the query menu
the default location is the current value x-mouse-pos."
  (x-mouse-query (or arg x-mouse-pos) question '(("yes" . t) ("no")) t))

(defun x-mouse-y-or-n-p (question &optional pos)
  "Ask user a yes or no question. No selection is a synonym for no.
Optional 2nd arg POS is a list (x y) of x and y coordinates for the query menu
the default location is the current value x-mouse-pos."
 (x-mouse-query (or pos x-mouse-pos) question '(("yes". t) ("no"))))

(defun x-mouse-completing-select (prompt table predicate
				  &optional require-match initial-input pos)
  "Offer a list of possibilities for selection using the mouse under X.
Args are PROMPT, TABLE, PREDICATE.
Optional args are REQUIRE-MATCH, INITIAL-INPUT and POS.
PROMPT is a string to prompt with; normally it ends in a colon and a space.
TABLE is an alist whose elements' cars are strings, or an obarray (see
try-completion).
PREDICATE limits completion to a subset of TABLE; see try-completion.
If REQUIRE-MATCH is non-nil, the user is not allowed to exit unless
a selection from TABLE is made.
If INITIAL-INPUT is non-nil, make this the default selection (the one
the mouse pointer is warped to).
If POS is non-nil, it is a list of (x-pos y-pos) for position on screen.
Don't try to use this as a replacement for completing-read: it's too slow and 
your screen is probably not big enough to list all possibilities; think about
C-h f."
  (let (possibilities)
    (message "Making completion list...")
    (if (or (vectorp table) (arrayp table))
	;; What is the canonical test for obarray-ness?
	(mapatoms 
	 (function 
	  (lambda (x)
	    (if (funcall predicate x)
		(setq possibilities (append possibilities (list x))))))
		  table)
      (if (consp table)
	  (mapcar
	   (function
	    (lambda (x)
	      (if (funcall predicate x)
		  (setq possibilities (append possibilities (list x))))))
		  table)))
    (message "Making completion list...done")
    (let ((selections
	   (if initial-input
	       (cons initial-input (delq initial-input possibilities))
	     possibilities)))
      (x-mouse-query (or pos x-mouse-pos) prompt selections require-match))))

