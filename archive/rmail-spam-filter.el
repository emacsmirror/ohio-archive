;; Emacs Lisp Archive Entry
;; Filename: rmail-spam-filter.el  
;; Version: 1.5
;; Created: Nov 15, 2000.
;; Keywords: email, spam, filter, rmail
;; Author: Eli Tziperman <eli@beach.weizmann.ac.il>
;; Description: filter spam in RMAIL based on sender, subject and/ or contents.
;; URL: http://www.weizmann.ac.il/~eli/Downloads/rmail-spam-filter/

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;;; Commentary:
;;; -----------

;;; Automatically recognize and delete junk email before it is
;;; displayed in rmail/rmail-summary.  Spam emails are defined by
;;; specifying one or more of the sender, subject and contents.

;;; Usage:
;;; ------

;;; put in your .emacs:

;;;   (load "rmail-spam-filter.el")
;;;   (setq rmail-display-summary t)

;;; and use customize (in rmail-spam-filter group) to:

;;; (1) turn on the variable rmail-use-spam-filter,

;;; (2) specify in variable rmail-spam-definitions-alist what sender,
;;; subject and contents make an email be considered spam.

;;; in addition, you may:

;;; (3) specify if blind-cc'ed mail (no "To:" header field) is to be
;;; treated as spam (variable rmail-spam-no-blind-cc; Thanks to Ethan
;;; Brown <ethan@gso.saic.com> for this).

;;; (4) specify if rmail-spam-filter should ignore case of spam
;;; definitions (variable rmail-spam-filter-ignore-case; Thanks to
;;; Ethan Brown <ethan@gso.saic.com> for the suggestion).

;;; (5) Specify a "white-list" of trusted senders. If any
;;; rmail-spam-white-list string matches a substring of the "From"
;;; header, the message is flagged as a valid, non-spam message (Ethan
;;; Brown <ethan@gso.saic.com>).

(require 'rmail)

;; For find-if and other cool common lisp functions we may want to use. (EDB)
(require 'cl)				

(if (not (fboundp 'rmail-make-summary-line)) (load-library "rmailsum"))

(defgroup rmail-spam-filter nil
  "Mail reader for Emacs."
  :group 'rmail)

(defcustom rmail-use-spam-filter nil
  "*Non-nil to activate the rmail spam filter.
Specify rmail-spam-definitions-alist to define what you consider spam
emails."
  :type 'boolean
  :group 'rmail-spam-filter )
 
(defcustom rmail-spam-file "~/XRMAIL-SPAM"
  "*name of rmail file for saving spam."
  :type 'string
  :group 'rmail-spam-filter )

(defcustom rmail-spam-no-blind-cc nil
  "*Non-nil to treat blind CC (no To: header) as spam."
  :type 'boolean
  :group 'rmail-spam-filter )

(defcustom rmail-spam-filter-ignore-case nil
  "*Non-nil to ignore case in rmail spam definitions alist."
  :type 'boolean
  :group 'rmail-spam-filter )

(defcustom rmail-spam-white-list nil
  "*List of strings to identify valid senders.  If any
rmail-spam-white-list string matches a substring of the 'From' header,
the message is flagged as a valid, non-spam message.  Example: If your
domain is emacs.com then including 'emacs.com' in your
rmail-spam-white-list would flag all mail from your colleagues as
valid."
  :type '(repeat string)
  :group 'rmail-spam-filter )

(defcustom rmail-spam-definitions-alist nil
  "*Alist matching strings defining spam.  Each definition may contain
specifications of one or more of the elements {subject, sender,
recipients or contents}, as well as a definition of what to do with
the spam (action item).  A spam e-mail is defined as one that fits all
of the specified elements of any one of the spam definitions."
  :type '(repeat 
          (list :format "%v"
	   (cons :format "%v" :value (from . "")
		 (const :format ""  from)
		 (string :tag "From"  ""))
	   (cons :format "%v" :value (to . "")
		 (const :format ""  to)
		 (string :tag "To"  ""))
	   (cons :format "%v" :value (subject . "")
		 (const :format ""  subject)
		 (string :tag "Subject"  ""))
	   (cons :format "%v" :value (contents . "")
		 (const :format ""  contents)
		 (string :tag "Contents"  ""))
	   (cons :format "%v" :value (action . output-and-delete)
		 (const :format "" action)
		 (choice :tag "Action selection" 
		  (const :tag "output to spam folder and delete" output-and-delete)
		  (const :tag "delete spam" delete-spam)
		  ))
   ))
  :group 'rmail-spam-filter)

(defun rmail-spam-filter (msg)
  " Return nil if msg is spam based on rmail-spam-definitions-alist.
If spam, output msg to a file rmail-spam-file and delete it from rmail
file.  Called for each message when summary is made as the FUNCTION
argument of new-rmail-summary."

  (let ((old-message)
	(return-value)
	(this-is-a-spam-email)
	(maybe-spam)
	(message-sender)
	(message-recepients)
	(message-subject)
	(num-spam-definition-elements)
	(num-element 0)
	(exit-while-loop nil)
	(saved-case-fold-search case-fold-search)
	(save-current-msg)
	)
  
  ;; is this a new message?
  (save-excursion
    (set-buffer rmail-buffer)
    (if (< msg rmail-current-message)
	(setq old-message t)
	(setq old-message nil)))
  ;;  use filter to look at msg only if it is a new one:
  (if old-message
      (setq return-value t)
    (save-excursion
      (save-restriction
	(setq this-is-a-spam-email nil)
	;; Narrow buffer to header of message and get Sender and
	;; Subject fields to be used below:
	(save-restriction
	  (goto-char (rmail-msgbeg msg))
	  (narrow-to-region (point) (progn (search-forward "\n\n") (point)))
	  (setq message-sender (mail-fetch-field "From"))
	  (setq message-recepients (mail-fetch-field "To"))
	  (setq message-subject (mail-fetch-field "Subject"))
	  )
	;; Find number of spam-definition elements in the list
	;; rmail-spam-definitions-alist specified by user:
	(setq num-spam-definition-elements (safe-length
					    rmail-spam-definitions-alist))

	;;; do we want to ignore case in spam definitions:
	  (setq case-fold-search rmail-spam-filter-ignore-case)
	
	;; Check for blind CC condition.  Set vars such that while
	;; loop will be bypassed and spam condition will trigger (EDB)
	(if (and rmail-spam-no-blind-cc
		 (null message-recepients))
	    (progn
	      (setq exit-while-loop t)
	      (setq maybe-spam t)
	      (setq this-is-a-spam-email t)))
	
	  ;; Check white list, and likewise cause while loop
	  ;;  bypass. (EDB)
	  (if (find-if '(lambda (white-str)
			  (string-match white-str message-sender))
		       rmail-spam-white-list)
	      (progn
		(setq exit-while-loop t)
		(setq maybe-spam nil)
		(setq this-is-a-spam-email nil)))
	    
	;; scan all elements of the list rmail-spam-definitions-alist
	(while (and
		(< num-element num-spam-definition-elements)
		(not exit-while-loop))
	  (progn
	    ;; Initialize maybe-spam which is set to t in one of two
	    ;; cases: (1) unspecified definition-elements are found in
	    ;; rmail-spam-definitions-alist, (2) empty field is found
	    ;; in the message being scanned (e.g. empty subject,
	    ;; sender, recepients, etc).  The variable is set to nil
	    ;; if a non empty field of the scanned message does not
	    ;; match a specified field in
	    ;; rmail-spam-definitions-alist.
	    (setq maybe-spam t)
	    ;; initialize this-is-a-spam-email to nil.  This variable
	    ;; is set to t if one of the spam definitions matches a
	    ;; field in the scanned message.
	    (setq this-is-a-spam-email nil)

	    ;; start scanning incoming message:
	    ;;---------------------------------
	    
	    ;; if sender field is not specified in message being
	    ;; scanned, AND if "from" field does not appear in spam
	    ;; definitions for this element, this may still be spam
	    ;; due to another element...
	    (if (and (not message-sender)
		     (string-match
		      (cdr (assoc 'from (nth num-element
					     rmail-spam-definitions-alist))) ""))
		(setq maybe-spam t)
	      ;; ... else, if message-sender does appear in the
	      ;; message, and it also appears in the spam definition
	      ;; list, it is potentially spam:
	      (if (and message-sender
		       (string-match
			(cdr (assoc 'from (nth num-element
					       rmail-spam-definitions-alist)))
			message-sender)
		       )
		  (setq this-is-a-spam-email t)
		(setq maybe-spam nil)
		)
	      )
	    ;; next, if spam was not ruled out already, check recepients:
	    (if maybe-spam
		;; if To field does not exist AND is not specified,
		;; this may still be spam due to another element...
		(if (and (not message-recepients)
			 (string-match
			  (cdr (assoc 'to
				      (nth num-element
					   rmail-spam-definitions-alist))) ""))
		    (setq maybe-spam t)
		  ;; ... else, if To field does appear in the message,
		  ;; and it also appears in spam definition list, this
		  ;; is potentially a spam:
		  (if (and message-recepients
			   (string-match
			    (cdr (assoc 'to (nth num-element
						 rmail-spam-definitions-alist)))
			    message-recepients)
			   )
		      (setq this-is-a-spam-email t)
		    (setq maybe-spam nil)
		    )
		  )
	      )
	    ;; next, if spam was not ruled out already, check subject:
	    (if maybe-spam
		;; if subject field does not exist AND is not
		;; specified, this may still be spam due to another
		;; element...
		(if (and (not message-subject)
			(string-match
			 (cdr (assoc 'subject
				     (nth num-element
					  rmail-spam-definitions-alist)))
			 ""))
		    (setq maybe-spam t)
		  ;; ... else, if subject field does appear in the
		  ;; message, and it also appears in the spam
		  ;; definition list, this is potentially a spam:
		  (if (and message-subject
			   (string-match
			    (cdr (assoc 'subject (nth num-element
						      rmail-spam-definitions-alist)))
			    message-subject)
			   )
		      (setq this-is-a-spam-email t)
		    (setq maybe-spam nil)
		    )
		  )
	      )
	    ;; next, if spam was not ruled out already, check
	    ;; contents: if contents field is not specified, this may
	    ;; still be spam due to another element...
	    (if maybe-spam
		(if (string-match
		     (cdr (assoc 'contents
				 (nth num-element
				      rmail-spam-definitions-alist))) "")
		    (setq maybe-spam t)
		  ;; ... else, check to see if it appears in spam
		  ;; definition:
		  (if (string-match
		       (cdr (assoc 'contents
				   (nth num-element
					rmail-spam-definitions-alist)))
		       (buffer-substring
			(rmail-msgbeg msg) (rmail-msgend msg)))
		      (setq this-is-a-spam-email t)
		    (setq maybe-spam nil)))
	      )
	    ;; if the search in rmail-spam-definitions-alist found
	    ;; that this email is spam, output the email to the spam
	    ;; rmail file, mark the email for deletion, leave the
	    ;; while loop and return nil so that an rmail summary line
	    ;; wont be displayed for this message:
	    (if (and this-is-a-spam-email maybe-spam)
		;; found that this is spam, no need to look at the
		;; rest of the rmail-spam-definitions-alist, exit
		;; loop:
		(setq exit-while-loop t)
	      ;; else, spam was not yet found, increment number of
	      ;; element in rmail-spam-definitions-alist and proceed
	      ;; to next element:
	      (setq num-element (+ num-element 1)))
	    )
	  )
	(if (and this-is-a-spam-email maybe-spam)
	    (progn
	      (message "Found spam!")
	      ;; (ding 1) (sleep-for 2)

	      ;; temprarily set rmail-current-message in order to
	      ;; output and delete the spam msg if needed:
	      (setq save-current-msg rmail-current-message)
	      (setq rmail-current-message msg)
	      ;; check action item and rmail-spam-definitions-alist
	      ;; and do it:
	      (cond
	       ((equal (cdr (assoc 'action
				   (nth num-element rmail-spam-definitions-alist)))
		       'output-and-delete)
		(progn
		  (rmail-output-to-rmail-file rmail-spam-file)
		  (rmail-delete-message)
		  ))
	       ((equal (cdr (assoc 'action
				   (nth num-element rmail-spam-definitions-alist)))
		       'delete-spam)
		(progn
		  (rmail-delete-message)
		  ))
	       )
	       (setq rmail-current-message save-current-msg)
	      ;; set return value.  These lines must be last in the
	      ;; function, so that they will determine the value
	      ;; returned by rmail-spam-filter:
	      (setq return-value nil))
	    (setq return-value t)))))
    (setq case-fold-search saved-case-fold-search)
    return-value))

(setq rmail-is-quitting nil)
(defun rmail-summary ()
  "Display a summary of all messages, one line per message."
  (interactive)
  (let
      ((looking-at-rmail-spam-file nil))
    ;; when using rmail to look at the rmail-spam-file (to make sure
    ;; all messages there are indeed not needed), turn the filter off:
    (if (string-match
	 (expand-file-name rmail-spam-file)
	 (expand-file-name (buffer-file-name rmail-buffer)))
	(setq looking-at-rmail-spam-file t))

    (if (and rmail-use-spam-filter
	     (not rmail-is-quitting)
	     (not looking-at-rmail-spam-file))
	(progn
	  (rmail-new-summary "All" '(rmail-summary) 'rmail-spam-filter)
	  ;; after reading new mail, display the first non-deleted new
	  ;; message, or if none, the last old message:
	  (set-buffer rmail-buffer)
	  (if (rmail-message-deleted-p rmail-current-message)
	      (progn
		(rmail-show-message (1- rmail-current-message))
		(rmail-next-undeleted-message 1))
	    (rmail-show-message rmail-current-message))
	  (pop-to-buffer rmail-summary-buffer))
      (rmail-new-summary "All" '(rmail-summary) nil)
      ))
    )

;; prevent calling spam filter when recalculating summary during
;; quitting of RMAIL.
(defadvice rmail-quit
  (before advice-rmail-quit-dont-use-spam-filter activate)
  " Make sure rmail-spam-filter is not used while calling
    rmail-summary during quitting of RMAIL."
  (setq rmail-is-quitting t)
  )

(defadvice rmail-quit
  (after advice-rmail-quit-dont-use-spam-filter activate)
  " Make sure rmail-spam-filter is not used while calling
    rmail-summary during quitting of RMAIL."
  (setq rmail-is-quitting nil)
  )


