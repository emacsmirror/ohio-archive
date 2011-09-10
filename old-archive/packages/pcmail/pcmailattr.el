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

(defvar pcmail-attr-obarray (make-vector 47 0)
  "An attribute obarray used for completion in attribute-manipulation
commands.")

;; attributes used by the mail reader.  Some may also be defined by 
;; particular folder formats

(mapcar '(lambda (a) (intern a pcmail-attr-obarray))
	'("deleted" "forwarded" "filed" "answered" "unseen" "recent" 
	  "badheader" "printed" "copied" "edited" "timely" "expired" 
	  "undigestified" "archived" "precious"))

;;; sticky defaults

(defvar pcmail-last-attr nil
  "The name of the last attribute given to an attribute command.")

;;;; attribute-changing operations: deletion, undeletion, general attribute 
;;;; setting and clearing.

(defun pcmail-set-message-attr-menu (event)
  "Set a named attribute of the current message.  
Args: (event)
  Pop up a window containing valid attributes, then set the selected
attribute."
  (interactive "e")
  (let ((choice (pcmail-read-attr "Set attribute: ")))
    (and choice (pcmail-change-message-attr choice t))))

(defun pcmail-clear-message-attr-menu (event)
  "Clear a named attribute of the current message.  
Args: (event)
  Pop up a window containing valid attributes, then clear the selected
attribute."
  (interactive "e")
  (let ((choice (pcmail-read-attr "Clear attribute: ")))
    (and choice (pcmail-change-message-attr choice nil))))

(defun pcmail-change-message-attr (attr mode)
  "Toggle a named attribute of the current message.  
Args: (attr mode)
  Toggle a named attribute of the current message.  Completion on input is
permitted; input defaults to last attribute given to an attribute command.
With a prefix arg, don't toggle.  If the arg is positive, set the attribute;
if negative, clear the attribute."
  (interactive
   (let ((mode) (prompt))
     (cond ((null current-prefix-arg) 
	    (setq mode 'toggle
		  prompt "Toggle attribute: "))
	   ((and (numberp current-prefix-arg)
		 (>= current-prefix-arg 0))
	    (setq mode t
		  prompt "Set attribute: "))
	   (t
	    (setq mode nil
		  prompt "Clear attribute: ")))
     (list (pcmail-read-attr prompt) mode)))
  (pcmail-barf-if-empty-folder)
  (pcmail-change-message-attr-1 attr mode pcmail-current-subset-message 1))

(defun pcmail-set-attr-subset-menu (event)
  "Set a named attribute in each message of the current message subset.
Args: (event)
  Pop up a window containing valid attributes, then set the selected
attribute."
  (interactive "e")
  (let ((choice (pcmail-read-attr "Set attribute: ")))
    (and choice (pcmail-change-attr-subset choice t))))

(defun pcmail-clear-attr-subset-menu (event)
  "Clear a named attribute in each message of the current message subset.
Args: (event)
  Pop up a window containing valid attributes, then clear the selected
attribute."
  (interactive "e")
  (let ((choice (pcmail-read-attr "Clear attribute: ")))
    (and choice (pcmail-change-attr-subset choice nil))))

(defun pcmail-change-attr-subset (attr mode)
  "Toggle a named attribute in each message of the current message subset.
Args: (attr mode)
  Toggle a named attribute in each message of the current message subset.
Completion on input is permitted; input defaults to last attribute given to
an attribute command.  With a prefix arg, don't toggle.  If the arg is
positive, set the attribute; if negative, clear the attribute."
  (interactive
   (let ((mode) (prompt))
     (cond ((null current-prefix-arg) 
	    (setq mode 'toggle
		  prompt "Toggle attribute: "))
	   ((and (numberp current-prefix-arg)
		 (>= current-prefix-arg 0))
	    (setq mode t
		  prompt "Set attribute: "))
	   (t
	    (setq mode nil
		  prompt "Clear attribute: ")))
     (list (pcmail-read-attr prompt) mode)))
  (pcmail-barf-if-empty-folder)
  (pcmail-change-message-attr-1 attr mode 1 (pcmail-current-subset-length)))

(defun pcmail-change-message-attr-1 (attr state start len)
  "Munge message attributes in the current message subset.
Args: (attr state start len)
  Set attribute ATTR to STATE for all messages in the current subset from START
for LEN messages.  If STATE is 'toggle, toggle the current attribute state."
  (let ((i start))
    (unwind-protect
	(while (< i (+ start len))
	  (pcmail-set-attribute (pcmail-make-absolute i) attr state)
	  (and (zerop (% (- (setq i (1+ i)) start) pcmail-progress-interval))
	       (message "%sing %s attribute...%d"
			(cond ((eq state 'toggle) "Toggl")
			      (state "Sett")
			      (t "Clear")) 
			attr (- i start))))
      (pcmail-update-folder-mode-line pcmail-current-subset-message))
    (and (>= (- i start) pcmail-progress-interval)
	 (message "%sing %s attribute...done (%d message%s)"
		  (cond ((eq state 'toggle) "Toggl")
			(state "Sett")
			(t "Clear")) 
		  attr (- i start) (pcmail-s-ending (- i start))))))

(defun pcmail-undelete-previous-message ()
  "Looking backward from the current message, clear the first deleted 
message's delete attribute.
Args: none"
  (interactive)
  (pcmail-barf-if-empty-folder)
  (let ((n (pcmail-next-subset-message-of-type 
	    nil nil 'include-current 'pcmail-has-attribute-p "deleted")))
    (cond (n
	    (pcmail-set-attribute (pcmail-make-absolute n) "deleted" nil)
	    (pcmail-goto-message n))
	  (t
	    (message "No previous deleted message in the current subset.")))))

(defun pcmail-undelete-subset ()
  "Undelete all messages in the current message subset.
Args: none"
  (interactive)
  (pcmail-barf-if-empty-folder)
  (pcmail-change-message-attr-1 "deleted" nil 1
				(pcmail-current-subset-length)))

(defun pcmail-delete-message (&optional dont-skip)
  "Delete this message and move to the next interesting message.
Args: (&optional dont-skip)
Delete this message and move to the next interesting message.  Deleted 
messages remain in the folder until the \\[pcmail-expunge-folder] command 
is given.  With a prefix argument, delete and move to the next message in the 
current subset whether or not it is interesting."
  (interactive "P")
  (pcmail-barf-if-empty-folder)
  (let ((n pcmail-current-subset-message))
    (pcmail-set-attribute (pcmail-make-absolute n) "deleted" t)
    (pcmail-next-message dont-skip)
    (and (= n pcmail-current-subset-message)
	 (pcmail-update-folder-mode-line n))))

(defun pcmail-delete-message-backward (&optional dont-skip)
  "Delete this message and move to the previous interesting message.
Args: (&optional dont-skip)
  Delete this message and move to the previous interesting message.  
Deleted messages remain in the folder until the \\[pcmail-expunge-folder] 
command is given.  With a prefix argument, delete and move to the previous 
message in the current subset whether or not it is interesting."
  (interactive)
  (pcmail-barf-if-empty-folder)
  (let ((n pcmail-current-subset-message))
    (pcmail-set-attribute (pcmail-make-absolute n) "deleted" t)
    (pcmail-previous-message dont-skip)
    (and (= n pcmail-current-subset-message)
	 (pcmail-update-folder-mode-line n))))

(defun pcmail-delete-subset ()
  "Delete all messages in the current message subset.
Args: none
  Delete all messages in the current message subset.  Deleted messages remain
in the folder until the \\[pcmail-expunge-folder] command is given."
  (interactive)
  (pcmail-barf-if-empty-folder)
  (pcmail-change-message-attr-1 "deleted" t 1 (pcmail-current-subset-length)))

(defun pcmail-zap-to-message ()
  "Delete all messages in the current subset from the current message forward.
Args: none
  Delete all messages in the current subset from the current message forward.
Deleted messages remain in the folder until the \\[pcmail-expunge-folder] 
command is given."
  (interactive)
  (pcmail-barf-if-empty-folder)
  (pcmail-change-message-attr-1 "deleted" t pcmail-current-subset-message
				(1+ (- (pcmail-current-subset-length)
				       pcmail-current-subset-message))))

;;;; attribute-hacking utilities

;;; two functions that work on messages with relative numbers.  A relative 
;;; message number is that message's index within the current subset.  Its
;;; absolute number is its index within the entire folder.

(defun pcmail-update-folder-mode-line (n)
  "Display message information in the mode line.
Args: (n)
  Set pcmail-display-info to string describing message with relative number N.
The string is formatted by the directives in pcmail-folder-mode-line-format.
See description of that variable for details.  The formatted string will be
displayed in the mode line."
  (setq pcmail-display-info
	(pcmail-format-string
	 pcmail-folder-mode-line-format
	 (list (list "s" '(lambda (n) n) n)
	       (list "S" '(lambda () (pcmail-current-subset-length)))
	       (list "e" '(lambda () 
			    (if (eq major-mode 'pcmail-edit-mode)
				"Editing "
			      "")))
	       (list "E" '(lambda (n)
			    (let ((abs (pcmail-make-absolute n)) (exp))
			      (if (and (pcmail-has-attribute-p abs "timely")
				       (setq exp 
					     (pcmail-message-expiration abs)))
				  (concat "Expires: " exp)
				"")))
		     n)
	       (list "n" 
		     '(lambda (n)
			(cond ((or (/= (pcmail-current-subset-length)
				       pcmail-total-messages)
				   (/= n (pcmail-make-absolute n)))
			       (format "[%d/%d]" (pcmail-make-absolute n)
				       pcmail-total-messages))
			      (t
			       ""))) n)
	       (list "%" '(lambda () "%"))
	       (list "f" '(lambda () pcmail-folder-name))
	       (list "a" 
		     '(lambda (n)
			(let ((attrs (aref pcmail-attr-vector 
					   (pcmail-make-absolute n))))
			  (if attrs
			      (mapconcat 'identity attrs ", ")
			    ""))) n)
	       (list "l" 
		     '(lambda (n) 
			(pcmail-message-line-count (pcmail-make-absolute n)))
		     n)
	       (list "c"
		     '(lambda (n) 
			(pcmail-message-char-count (pcmail-make-absolute n)))
		     n)
	       (list "p"
		     '(lambda (n) 
			(let ((p (pcmail-message-priority
				  (pcmail-make-absolute n))))
			  (if (= p 1) ""
			    (format "Priority: %d" p))))
		     n))))
  (and (= n pcmail-current-subset-message)
       (pcmail-force-mode-line-update)))

(defun pcmail-next-subset-message-of-type (forward-p invert-p include-current-p
					   pred &rest args)
  "Return the number of the next message that satisfies a predicate.
Args: (forward-p invert-p include-current-p pred &rest args)
  Starting with the current subset message if INCLUDE-CURRENT-P is non-nil,
the first message after/before current otherwise, return the number of the
first subset message that satisfies PRED applied to ARGS, or if INVERT-P is
non-NIL, does not satisfy PRED applied to ARGS.  Search forward
if FORWARD-P is non-nil, backward else.  If no such message is found,
return NIL."
  (let ((found) (current pcmail-current-subset-message))
    (or include-current-p
	(setq current (funcall (if forward-p '1+ '1-) current)))
    (while (and (not found) 
		(funcall (if forward-p '<= '>=) current 
			 (if forward-p (pcmail-current-subset-length) 1)))
      (and (if invert-p
	       (not (apply pred (pcmail-make-absolute current) args))
	     (apply pred (pcmail-make-absolute current) args))
	   (setq found current))
      (setq current (funcall (if forward-p '1+ '1-) current)))
    found))

;;; all following routines deal with absolute-numbered messages


(defun pcmail-interesting-p (n)
  "Return non-NIL if message absolute-numbered N is interesting, NIL else.
Args: (n)
  Return non-NIL if message absolute-numbered N is interesting, NIL else.  
Message N is interesting if pcmail-interesting-hook returns non-NIL 
when applied to N.  If pcmail-interesting-hook is NIL, all messages 
are interesting."
  (if pcmail-interesting-hook
      (funcall pcmail-interesting-hook n)
    t))

(defun pcmail-has-attribute-p (n attr)
  "Check an attribute's membership in a message attribute list.
Args: (n attr)
  Return T if ATTR is a member of message absolute-numbered N's attribute 
list, NIL else."
  (pcmail-in-sequence-p attr (aref pcmail-attr-vector n)))

(defun pcmail-set-attribute (n attr state)
  "Set, clear, or toggle a message attribute.
Args: (n attr state)
  Set message absolute-numbered N's attribute ATTR to STATE.  If STATE is 
'toggle, toggle the attribute's state.  If ATTR is \"deleted\", do not
set state to non-NIL if N already has the \"precious\" attribute"
  (pcmail-barf-if-empty-folder)
  (or (pcmail-attribute-p attr)
      (error "No attribute named %s." attr))
  (let ((curstate (pcmail-folder-message-has-attr-p n attr)))
    (cond ((and curstate (or (eq state 'toggle) (not state)))
	   (pcmail-change-message-attribute-list n attr nil)
	   (pcmail-change-folder-message-attr n attr nil))
	  ((and (not curstate) state
		(if (and (pcmail-has-attribute-p n "precious")
			 (string= attr "deleted"))
		    nil
		  t))
	   (pcmail-change-message-attribute-list n attr t)
	   (pcmail-change-folder-message-attr n attr t)
	   (and (string= attr "deleted")
		(setq pcmail-folder-needs-expunge-p t)))))
  
  ; this may have screwed up the region around the current message.  Fix it.
  (pcmail-narrow-to-message 
   (pcmail-make-absolute pcmail-current-subset-message))
  state)

(defun pcmail-legal-attribute-name-p (a)
  "Return non-NIL if A is a legal attribute string, NIL else.
Args: (a)"
  (not (string-match "," a)))

;;; timely message support

(defun pcmail-hack-timely-messages (tl)
  "Given a list of timely messages, figure out what to do with them.
Args: (tl)
  TL is a list of message numbers, corresponding to messages with the 
\"timely\" attribute set.  If any of these messages has an expired: header
field earlier than the current date, apply pcmail-expiration-hook to
the message number."
  (let ((now-days (pcmail-date-triple-to-ndays 
		   (pcmail-string-to-date-triple))))
    (mapcar 
     '(lambda (n)
	(let ((expiration (pcmail-message-expiration n)))
	  (cond ((and expiration
		      pcmail-expiration-hook
		      (not (pcmail-has-attribute-p n "expired"))
		      (setq expiration 
			    (pcmail-string-to-date-triple expiration))
		      (setq expiration
			    (pcmail-date-triple-to-ndays expiration))
		      (<= expiration now-days))
		 (funcall pcmail-expiration-hook n)
		 (pcmail-set-attribute n "expired" t)
		 (pcmail-set-attribute n "timely" nil)))))
     tl)))

(defun pcmail-set-message-expiration (n date)
  "Set message absolute-numbered N's expiration to DATE.
Args: (n date)
 N is an absolute message number, DATE is a date string dd-mmm-yy.
Remove N's expires: field if it has one.  If DATE is non-nil, place its
entries in a new expires: field."
  (save-excursion
    (save-restriction
      (pcmail-narrow-to-unpruned-header n)
      (goto-char (point-min))
      (let ((buffer-read-only nil)
	    (case-fold-search t))
	(and (re-search-forward "^expires:.*\n\\([ \t]+.*\n\\)*" nil t)
	     (replace-match ""))
	(and date (insert "Expires: " date "\n"))))))

(defun pcmail-message-expiration (n)
  "Return message absolute-numbered N's expiration date as a date string
Args: (n)
  If N has a a valid expired: field in the form \"dd mm yy\", return it, 
else return NIL."
  (save-excursion
    (save-restriction
      (pcmail-narrow-to-unpruned-header n)
      (goto-char (point-min))
      (mail-fetch-field "expires"))))

;;; utilities which know how folder attribute names are stored.  All
;;; the following are internal to pcmailattr.el
;;;
;;; system-defined attributes are interned in a completion obarray at load 
;;; time.  New user-defined attributes are interned into the obarray as 
;;; needed, as well as installed in the current folder's babyl header 
;;; labels: field.  Old user-defined attributes are read from labels: fields 
;;; and interned into the obarray as folders are opened for the first time

(defun pcmail-change-message-attribute-list (n attr state)
  "Add (STATE = T) or remove (STATE = NIL) an attribute from an attribute list.
Args (n attr state)
  If STATE is non-NIL, add attribute string ATTR to message absolute-numbered
N's attribute list, otherwise remove it."
  (cond (state
	 (aset pcmail-attr-vector n 
	       (cons attr (aref pcmail-attr-vector n))))
	(t
	 (let ((attrs (aref pcmail-attr-vector n))  ; need a delq variant!
	       (temp))
	   (while attrs
	     (or (string= attr (car attrs))
		 (setq temp (cons (car attrs) temp)))
	     (setq attrs (cdr attrs)))
	   (aset pcmail-attr-vector n temp)))))

(defun pcmail-read-attr (pr)
  "Read an attribute from the minibuffer.
Args: (pr)
  Read an attribute from the minibuffer, prompting with PR.  Blank input
causes the value of pcmail-last-attr to be used.  Non-blank input completes
off pcmail-attr-obarray, setting pcmail-last-attr to be the input just
received.  If the attribute is not in the obarray ask if it should be put
there as well as in the current folder's Babyl header labels: field."

  (or (pcmail-attribute-p pcmail-last-attr)
      (setq pcmail-last-attr nil))
  (let* ((a) 
	 (rk (recent-keys)) 
	 (event (aref rk (- (length rk) 2)))
	 (attrs (mapcar '(lambda (x) (list x x)) 
			(all-completions "" pcmail-attr-obarray 'identity))))

    ;; I *strongly* suspect there is a better way to get the most recent
    ;; mouse event than getting the next-most-recent event.
    (setq a
	  (if (listp event)
	      (or (car (x-popup-menu (if (listp event) event
				       (cons '(0 0) (selected-frame)))
				     (list "Select Attribute"
					   (cons "Select Attribute"
						 (sort attrs
						       '(lambda (a b) 
							  (string< (car a) 
								   (car b))))
						 ))))
		  (keyboard-quit))
	    (pcmail-completing-read pr pcmail-attr-obarray pcmail-last-attr)))
    (or (pcmail-attribute-p a)
	(pcmail-create-user-defined-attr a))
    (setq pcmail-last-attr a)))

(defun pcmail-attribute-p (a)
  "Return non-NIL if A is a valid attribute, NIL else.
Args: (a)"
  (and (stringp a) (intern-soft a pcmail-attr-obarray)))

(defun pcmail-load-user-defined-attrs ()
  "Intern user-defined labels.
Args: none"
  (mapcar '(lambda (x) (intern x pcmail-attr-obarray))
	  (pcmail-user-defined-attr-list)))

(defun pcmail-create-user-defined-attr (a)
  "Install undefined attribute A in the current and primary folders.
Args: (a)"
  (cond ((y-or-n-p "Undefined attribute; install? ")
	 (or (pcmail-legal-attribute-name-p a)
	     (error "%s is not a legal attribute name." a))
	 (save-excursion
	   (pcmail-open-folder pcmail-folder-name)
	   (pcmail-install-user-defined-attr a))
	 (and (not (string= pcmail-folder-name pcmail-primary-folder-name))
	      (save-excursion
		(pcmail-open-folder pcmail-primary-folder-name)
		(pcmail-install-user-defined-attr a)))

	; insert may have screwed up region around current message.  Fix it.
	 (pcmail-narrow-to-message 
	  (pcmail-make-absolute pcmail-current-subset-message))
	 (intern a pcmail-attr-obarray))
	(t
	 (error "Aborted."))))
		     
(provide 'pcmailattr)

