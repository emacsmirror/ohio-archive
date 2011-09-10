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

;;;; global variables

;;; system-defined globals

(defvar pcmail-filter-alist
  '(("address" (pcmail-has-address-p pcmail-last-addresses)
	       (setq pcmail-last-addresses
		     (pcmail-read-string-default "Addresses: " 
						 pcmail-last-addresses)))
    ("all" t)
    ("attribute" (pcmail-contains-attribute-p pcmail-last-filter-attr)
		 (setq pcmail-last-filter-attr
		       (pcmail-read-attr "Attribute: ")))
    ("date-range" (pcmail-within-date-range-p pcmail-last-date-range)
		  (setq pcmail-last-date-range (pcmail-read-date-range)))
    ("interesting" (pcmail-subset-interesting-message-p))
    ("numeric-range" (pcmail-within-numeric-range-p pcmail-last-numeric-range)
		     (setq pcmail-last-numeric-range 
			   (pcmail-read-numeric-range)))
    ("string" (pcmail-contains-string-p pcmail-last-search)
	      (setq pcmail-last-search
		    (pcmail-read-string-default "Search string (regexp): "
						pcmail-last-search)))
    ("todays" (pcmail-within-date-range-p (list pcmail-today pcmail-today))
	      (setq pcmail-today (pcmail-string-to-date-triple)))
    ("topic" (pcmail-has-subject-p pcmail-last-subject)
             (setq pcmail-last-subject
		   (pcmail-read-string-default "Subject line (regexp): "
					       pcmail-last-subject)))
    ("unanswered" (not (pcmail-contains-attribute-p "answered")))
    ("unseen" (pcmail-contains-attribute-p "unseen")))
  "List of filter names, expressions, and setup functions.  When using
a particular filter, the setup function (if non-NIL) is run through
eval to set up any arguments needed by the filter.  Then each message
in the folder is applied to the filter expression.  The variable
pcmail-current-tested-message is bound to the absolute number of the
message being tested before the function is called.  This allows the
function to access the current absolute message number without
requiring that it do so.  The filter expression is run through eval
because the expression arguments must be evaluated at filter time and
can change on successive applications of the filter.  For each message
run through the filter, if the filter expression evaluates non-NIL,
the message is included in the resulting subset.")

;;; defaults

(defvar pcmail-current-filter-description t
  "Current filter expression.")

(defvar pcmail-last-search nil
  "The last regular expression given to a search command.")

(defvar pcmail-last-addresses nil
  "The last comma-separated list of addresses given to an address command.")

(defvar pcmail-last-subject nil
  "The last subject given to a topic filter.")

(defvar pcmail-last-numeric-range nil
  "The last numeric range given to a numeric range command.  A numeric range
is a list of two numbers, low end and high end.")

(defvar pcmail-last-date-range nil
  "The last date range given to a date range command.  A date range is a pair
of triples (day month year), low end and high end.")

(defvar pcmail-last-filter-name nil
  "The last filter name given to a filter command.")

;;;; subset maintenance commands and utilities

;;; subset commands

(defun pcmail-filter-folder (filter-name invert)
  "Run the current folder through a specified filter.
Args: (filter-name invert)
  Get a filter name and associated arguments from the minibuffer.  Completion 
of input is permitted; input defaults to last filter name requested.  Apply
the filter's predicate to each message in the current folder.  Messages
which pass through the filter comprise the current subset and are the only
accessible messages in the current folder.  With a prefix argument,
invert the filter so the current subset is those messages NOT passing the
filter.  In both cases, if the desired subset is empty, do nothing.  
User-defined filters are defined in your emacs startup file using the
pcmail-define-filter function."
  (interactive
   (list (pcmail-read-filter-name 
	  (and current-prefix-arg "(Inverted) filter name: "))
	 current-prefix-arg))
  (cond ((pcmail-build-subset-membership 
	  (pcmail-get-filter filter-name) nil invert)
	 (pcmail-goto-message 1)
	 (pcmail-maybe-resummarize-folder))
	(t
	 (error "Desired subset is empty."))))

(defun pcmail-expand-subset ()
  "Expand the current subset to include all messages in the current folder.
Args: none"
  (interactive)
  (let ((n (pcmail-make-absolute pcmail-current-subset-message)))
    (pcmail-build-subset-membership t nil)
    (pcmail-goto-message n)
    (pcmail-maybe-resummarize-folder)))

(defun pcmail-define-filter (name sexp input-fn)
  "Install a user-defined filter.
Args: (name sexp input-fn)
  Create a filter entry named NAME with description SEXP and argument-input
function INPUT-FN, and install it in the assoc list pcmail-filter-alist.  
If a filter by that name already exists, ask for overwrite permission unless 
the name is the special filter named \"all\", in which case overwriting is not
permitted."
  (and (string= name "all")
       (error "Cannot overwrite the \"all\" filter"))
  (let ((ent))
    (and (setq ent (pcmail-filter-exists-p name))
	 (if (y-or-n-p "Filter exists; overwrite? ")
	     (setq pcmail-filter-alist (delq ent pcmail-filter-alist))
	   (error "Aborted.")))
    (setq pcmail-filter-alist
	  (cons (list name sexp input-fn) pcmail-filter-alist))))

;;; subset utility routines

(defun pcmail-build-subset-membership (pred start &optional invert)
  "Create a subset of messages that satisfy PRED
Args: (pred start &optional invert)
  Using filter description PRED, build a vector of messages that 
satisfy that description.  If START is NIL, begin at message 1, replacing 
the current subset with the subset generated by this function (unless it is
of zero length).  If START is non-NIL, begin membership testing at message 
START, appending any new members to the current subset.  If INVERT is not
NIL, invert the sense of the filter, placing messages NOT passing the filter
into the subset."
  (condition-case nil
      (let ((pcmail-current-tested-message (or start 1))
	    (subset))
	(while (<= pcmail-current-tested-message pcmail-total-messages)
	  (and (if invert (not (eval pred)) (eval pred))
	       (setq subset (cons pcmail-current-tested-message subset)))
	  (and (zerop (% (- (setq pcmail-current-tested-message 
				  (1+ pcmail-current-tested-message))
			    (or start 1))
			 pcmail-progress-interval))
	       (message "Checking filter membership...%d" 
			pcmail-current-tested-message)))
	(and (>= (- pcmail-current-tested-message (or start 1))
		 pcmail-progress-interval)
	     (message "Checking filter membership...done (%d message%s)"
		      (length subset) (pcmail-s-ending (length subset))))
	(and (or subset (eq pred t))
	     (setq pcmail-current-filter-description pred
		   pcmail-current-subset-vector 
		   (vconcat (if start
				pcmail-current-subset-vector
			      (make-vector 1 0))
			    (apply 'vector (nreverse subset)))))
	subset)
    (quit
      nil)))

(defun pcmail-fix-expunged-subset (map)
  "Remove expunged messages from the current subset
Args: (map)
  MAP is a vector pcmail-total-messages long, with entries that are either
a message's post-expunge message number, or NIL if the message was expunged.
This function updates the current subset vector's message numbers to their
post-expunged values."
  (let ((new-subset)
	(map-ent)
	(i 0))
    (unwind-protect
	(while (< i (length pcmail-current-subset-vector))
	  (setq map-ent (aref map (aref pcmail-current-subset-vector i)))
	  (and map-ent
	       (setq new-subset (cons map-ent new-subset)))
	  (setq i (1+ i)))
      (setq pcmail-current-subset-vector 
	    (apply 'vector (nreverse new-subset))))))

(defun pcmail-make-absolute (n)
  "Return Nth subset message's absolute message number
Args: (n)
  Convert relative message number N into an absolute number by indexing into
the current subset membership vector.  If N is larger than the current
subset length, return last subset message's absolute number.  If no absolute 
exists, return 0."
  (setq n (min n (pcmail-current-subset-length)))
  (or (aref pcmail-current-subset-vector n) 0))

(defun pcmail-filter-description (name)
  "Return named filter's description.  Signal an error if filter not found
Args: (name)"
  (let ((ent (pcmail-filter-exists-p name)))
    (or ent
	(error "No filter named %s" name))
    (nth 1 ent)))

(defun pcmail-filter-exists-p (name)
  "If NAME is a valid filter, return its assoc list entry, else NIL.
Args: (name)"
  (pcmail-search-entry-list name pcmail-filter-alist))
    
(defun pcmail-current-subset-length ()
  "Return the number of messages in the current subset.
Args: none"
  (1- (length pcmail-current-subset-vector)))

(defun pcmail-read-filter-name (&optional pr)
  "Read a filter name from the minibuffer.
Args: (&optional pr)
Read a filter name from the minibuffer.  Completion is permitted; input
defaults to pcmail-last-filter-name.  Signal an error if supplied filter
name is invalid."
  (let* ((s) (rk (recent-keys)) (event (aref rk (- (length rk) 2))))
    ;; I *strongly* suspect there is a better way to get the most recent
    ;; mouse event than getting the next-most-recent event.
    (setq s
	  (if (listp event)
	      (or 
	       (car (x-popup-menu event 
				  (list "Select Filter"
					(cons "Select Filter" 
					      (mapcar '(lambda (l)
							 (list (car l) 
							       (car l)))
						      pcmail-filter-alist)))))
	       (keyboard-quit))
	    (pcmail-completing-read (or pr "Filter name: ") pcmail-filter-alist
				    pcmail-last-filter-name)))
    (or (pcmail-filter-exists-p s)
	(error "No filter named %s." s))
    (setq pcmail-last-filter-name s)))

(defun pcmail-get-filter (filter-name)
  "Read filter arguments and return filter predicate.
Args: (filter-name)
  If FILTER-NAME is a valid filter, get its required arguments from the 
minibuffer and return the filter predicate."
  (let ((ent))
    (setq ent (pcmail-filter-exists-p filter-name))
    (and (nth 2 ent) (eval (nth 2 ent)))
    (nth 1 ent)))

;;; predicates for subset creation.  Each predicate is applied to a 
;;; message with number pcmail-current-tested-message.  This variable 
;;; is a free variable. 

(defun pcmail-has-address-p (recipients)
  "Return non-NIL if current message contains the supplied address regexp .
Args: (recipients)
  Convert comma-separated list of recipients RECIPIENTS into a regular
expression.  Return non-NIL if message pcmail-current-tested-message
(free variable) contains this regular expression, NIL else."
  (setq recipients (mail-comma-list-regexp recipients))
  (save-excursion
    (save-restriction
      (pcmail-narrow-to-unpruned-header pcmail-current-tested-message)
      (or (string-match recipients (or (mail-fetch-field "to") ""))
	  (string-match recipients (or (mail-fetch-field "resent-to") ""))
	  (string-match recipients (or (mail-fetch-field "from") ""))
	  (string-match recipients (or (mail-fetch-field "resent-from") ""))
	  (string-match recipients (or (mail-fetch-field "cc") ""))
	  (string-match recipients (or (mail-fetch-field "resent-cc") ""))))))

(defun pcmail-has-subject-p (subject)
  "Return non-NIL if current message contains the supplied subject regexp .
Args: (subject)"
  (save-excursion
    (save-restriction
      (pcmail-narrow-to-unpruned-header pcmail-current-tested-message)
      (string-match subject (or (mail-fetch-field "subject") "")))))

(defun pcmail-contains-attribute-p (attr)
  "Return non-NIL if current message has attribute, NIL else.
Args: (attr)
  Return non-NIL if pcmail-current-tested-message (free variable) has
ATTR set, NIL else."
  (pcmail-has-attribute-p pcmail-current-tested-message attr))

(defun pcmail-subset-interesting-message-p ()
  "Return non-NIL is current message is interesting, NIL else.
Args: none"
  (pcmail-interesting-p pcmail-current-tested-message))

(defun pcmail-within-numeric-range-p (range)
  "Return non-NIL if current message is within a numeric range, NIL else.
Args: (range)
  Return non-NIL if pcmail-current-tested-message (free variable) is
within the range of absolute message numbers specified by the list RANGE, 
NIL else."
  (and (>= pcmail-current-tested-message (nth 0 range))
       (<= pcmail-current-tested-message (nth 1 range))))

(defun pcmail-within-date-range-p (range)
  "Return non-NIL if the current message's date is within date range, NIL else.
Args: (range)
  Return non-NIL if pcmail-current-tested-message (free variable)
has its date within the range of dates specified by the list RANGE, NIL else.
Dates are triples (day month year); RANGE is a pair of such triples."
  (let ((lo (pcmail-date-triple-to-ndays (nth 0 range)))
	(hi (pcmail-date-triple-to-ndays (nth 1 range)))
	(date (pcmail-message-date pcmail-current-tested-message)))
    (and date
	 (setq date (pcmail-date-triple-to-ndays date))
	 (<= date hi)
	 (>= date lo))))

(defun pcmail-contains-string-p (regexp)
  "Return non-NIL if the current message contains a specified regexp, NIL else.
Args: (regexp)"
  (save-excursion
    (save-restriction
      (let ((case-fold-search t))
	(pcmail-narrow-to-message pcmail-current-tested-message)
	(re-search-forward regexp nil t)))))

;;; read ranges from keyboard

(defun pcmail-read-date-range ()
  "Read a date range from the minibuffer
Args: none
  Read a pair of dates from the minibuffer.  Dates must be input in the
form dd-mmm-yy.  Default range is pcmail-last-date-range, which is a pair of 
date triples, low and high.  If no default has been specified, use low value 
as default for high value.  If the string \"begin\" is input at the low value 
prompt, range includes all messages below high-value.  If the string \"now\" 
is input at the high value prompt, range includes all messages above 
low-value.  Input becomes new value of pcmail-last-date-range."

  ; our date input parser is stupid, so temporarily bind the date format to
  ; the date input format so default input works correctly
  (let ((lo) (hi) (pcmail-date-format "%d-%m-%y"))
    (setq lo
	  (pcmail-read-string-default 
	    "First date in range: " 
	    (and (nth 0 pcmail-last-date-range)
		 (pcmail-date-triple-to-string (nth 0 pcmail-last-date-range)))
	    t))
    (cond ((string= lo "begin")
	   (setq lo '(1 1 0)))
	  ((not (setq lo (pcmail-string-to-date-triple lo)))
	   (error "Date not dd-mmm-yy or \"begin\".")))
    (setq hi
	  (pcmail-read-string-default 
	    "Last date in range: " 
	    (if (nth 1 pcmail-last-date-range)
		(pcmail-date-triple-to-string (nth 1 pcmail-last-date-range))
		    (pcmail-date-triple-to-string lo))
	    t))
    (cond ((string= hi "now")
	   (setq hi (pcmail-string-to-date-triple)))
	  ((not (setq hi (pcmail-string-to-date-triple hi)))
	   (error "Date not dd-mmm-yy or \"now\".")))
    (if (> (pcmail-date-triple-to-ndays lo) (pcmail-date-triple-to-ndays hi))
	(list hi lo)
      (list lo hi))))

(defun pcmail-read-numeric-range ()
  "Read a numeric range from the minibuffer.
Args: none
  Read a pair of absolute message numbers from the minibuffer.  Default 
range is value of variable pcmail-last-numeric-range, which is a pair of 
numbers, low and high.  If no default has been specified, use low value as 
default for high value.  If the string \"first\" is input at the low
value prompt, range includes all messages below high-value.  If the string
\"last\" is input at the high value prompt, range includes all messages above
low-value.  Input becomes new value of pcmail-last-numeric-range."
  (let ((lo)
	(hi))
    (setq lo
	  (pcmail-read-string-default 
	    "First message in range: " 
	    (and (nth 0 pcmail-last-numeric-range)
		 (int-to-string (nth 0 pcmail-last-numeric-range)))
	    t))
    (cond ((string= lo "first")
	   (setq lo 1))
	  ((or (not (setq lo (string-to-int lo)))
	       (< lo 1)
	       (> lo pcmail-total-messages))
	   (error "Range endpoint not 1 - %d or \"first\"."
		  pcmail-total-messages)))
    (setq hi
	  (pcmail-read-string-default 
	    "Last message in range: " 
	    (if (nth 1 pcmail-last-numeric-range)
		(int-to-string (nth 1 pcmail-last-numeric-range))
	      (int-to-string lo))
	    t))
    (cond ((string= hi "last")
	   (setq hi pcmail-total-messages))
	  ((or (not (setq hi (string-to-int hi)))
	       (< hi 1)
	       (> hi pcmail-total-messages))
	   (error "Range endpoint not 1 - %d or \"last\"."
		  pcmail-total-messages)))
    (list (min lo hi) (max lo hi))))

(provide 'pcmailsub)
