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

;;;; movement and display commands within a single folder

;;; movement commands

(defun pcmail-beginning-of-message ()
  "Move to the beginning of the current message.
Args: none"
  (interactive)
  (pcmail-barf-if-empty-folder)
  (pcmail-goto-message pcmail-current-subset-message))

(defun pcmail-goto-message (&optional n)
  "Move to message number N of the current subset and display it.
Args: (&optional n)
Display message N in the current folder's curent subset.  If called
interactively, N is specified by a numeric prefix argument.  If not 
specified, N defaults to the first message in the subset."
  (interactive "p")
  (pcmail-display-subset-message (or n (setq n 1))))

(defun pcmail-scroll-goto-message (event)
  "Jump to a message based on the cursor's position in the scroll bar.
Args: (event)
Accept EVENT and move to the appropriate message in the folder."
  (interactive "e")
  (let ((window) (portion-whole) (position) 
	(modifier (car-safe (event-modifiers (car-safe event)))))
    (cond ((eq modifier 'click)
	   (setq position (event-start event)
		 window (posn-window position)
		 portion-whole (posn-col-row position))
	   (pcmail-scroll-goto-message-1 window portion-whole)))))

(defun pcmail-scroll-goto-message-1 (window portion-whole)
  (save-excursion
    (set-buffer (window-buffer window))
    (pcmail-goto-message (scroll-bar-scale
			  portion-whole
			  (pcmail-current-subset-length)))
    (goto-char (point-min))))

(defun pcmail-last-message (&optional dont-skip)
  "Move to the last interesting message in the current subset and display it.
Args: (&optional dont-skip)
  Display the last interesting message in the current folder's current subset.
pcmail-interesting-p returns non-NIL when applied to an interesting message.  
If called interactively, a prefix argument means move to the last message in 
the subset whether interesting or not."
  (interactive "P")
  (pcmail-barf-if-empty-folder)
  (let ((n))
    (cond (dont-skip
	    (setq n (pcmail-current-subset-length)))
	  (t
	    (let ((pcmail-current-subset-message
		    (pcmail-current-subset-length)))
	      (setq n (pcmail-next-subset-message-of-type 
			nil nil 'include-current 'pcmail-interesting-p)))))
    (cond (n
	   (pcmail-goto-message n))
	  (t
	   (pcmail-goto-message 1)
	   (message "No interesting messages in this folder")))))

(defun pcmail-next-message (&optional dont-skip)
  "Move to the next interesting message in the current subset and display it.
Args: (&optional dont-skip)
  Display the next interesting message in the current folder's current subset.
pcmail-interesting-p returns non-NIL when applied to an interesting message.  
If called interactively, a prefix argument means move to the next message in 
the subset whether interesting or not."
  (interactive "P")
  (pcmail-barf-if-empty-folder)
  (let ((n))
    (cond (dont-skip
	    (setq n (1+ pcmail-current-subset-message)))
	  (t
	    (setq n (pcmail-next-subset-message-of-type
		      'forward nil nil 'pcmail-interesting-p))))
    (cond (n
	    (pcmail-goto-message n))
	  (t
	    (message "No further interesting messages.")))))

(defun pcmail-next-message-of-type (filter-name invert)
  "Move to the next message in the current subset that satisfies a predicate.
Args: (filter invert)
  If called interactively, read a filter name from the minibuffer, use
it to read that filter's arguments and get the filter predicate.  If called 
as a function, supply a valid filter name.  Move to and display the next 
such message.  With a prefix argument, invert the filter sense."
  (interactive
   (list (pcmail-read-filter-name 
	  (concat "Show next message in"
		  (if current-prefix-arg " (inverted)" "")
		  " filter: "))
	 current-prefix-arg))
  (pcmail-barf-if-empty-folder)
  (let ((i (1+ pcmail-current-subset-message))
	(found)
	(pred (pcmail-get-filter filter-name))
	(pcmail-current-tested-message)) ;inherited by predicates
    (while (and (not found) (<= i (pcmail-current-subset-length)))
      (setq pcmail-current-tested-message (pcmail-make-absolute i))
      (and (if invert (not (eval pred)) (eval pred))
	   (setq found i))
      (setq i (1+ i)))
    (cond (found
	   (pcmail-goto-message found))
	  (t
	   (error "No more such messages in the current subset.")))))

(defun pcmail-previous-message (&optional dont-skip)
  "Move to the previous interesting message in the current subset and display.
Args: (&optional dont-skip)
  Display the previous interesting message in the current folder's current 
subset.  pcmail-interesting-p returns non-NIL when applied to an interesting 
message.  If called interactively, a prefix argument means move to the 
previous message in the subset whether interesting or not."
  (interactive "P")
  (pcmail-barf-if-empty-folder)
  (let ((n))
    (cond (dont-skip
	    (setq n (1- pcmail-current-subset-message)))
	  (t
	    (setq n (pcmail-next-subset-message-of-type
		      nil nil nil 'pcmail-interesting-p))))
    (cond (n
	    (pcmail-goto-message n))
	  (t
	    (message "No previous interesting messages.")))))

(defun pcmail-previous-message-of-type (filter-name invert)
  "Move to the previous message in the current subset satisfying a predicate.
Args: (filter invert)
  If called interactively, read a filter name from the minibuffer, use
it to read that filter's arguments and get the filter predicate.  If called 
as a function, supply a valid filter name.  Move to and display the first
previous such message.  With a prefix argument, invert the filter sense."
  (interactive
   (list (pcmail-read-filter-name 
	  (concat "Show previous message in"
		  (if current-prefix-arg " (inverted)" "")
		  " filter: "))
	 current-prefix-arg))
  (pcmail-barf-if-empty-folder)
  (let ((i (1- pcmail-current-subset-message))
	(found)
	(pred (pcmail-get-filter filter-name))
	(pcmail-current-tested-message)) ;inherited by predicates
    (while (and (not found) (>= i 1))
      (setq pcmail-current-tested-message (pcmail-make-absolute i))
      (and (if invert (not (eval pred)) (eval pred))
	   (setq found i))
      (setq i (1- i)))
    (cond (found
	   (pcmail-goto-message found))
	  (t
	   (error "No previous such messages in the current subset.")))))

;;; movement utility routines

(defun pcmail-display-subset-message (n)
  "Display the Nth message in the current subset.
Args: (n)"
  (let ((msg)
	(absolute))
    (cond ((< n 1)
	   (setq n (min 1 (pcmail-current-subset-length))
		 msg "Beginning of folder")
	   (setq pcmail-current-subset-message 1))
	  ((> n (pcmail-current-subset-length))
	   (setq n (pcmail-current-subset-length)
		 msg "End of folder")
	   (setq pcmail-current-subset-message 
		 (pcmail-current-subset-length)))
	  (t
	    (setq pcmail-current-subset-message n)))
    (setq absolute (pcmail-make-absolute n))
    (or (pcmail-header-pruned-p absolute)
	(pcmail-prune-header absolute t))
    (and (pcmail-has-attribute-p absolute "unseen")
	 (pcmail-set-attribute absolute "unseen" nil))    
    (pcmail-narrow-to-message absolute)
    (pcmail-update-folder-mode-line n)
    (and msg (message msg))))

(defun pcmail-message-char-count (n)
  "Return number of characters in message absolute-numbered N.
Args: (n)"
  (save-excursion
    (save-restriction
      (pcmail-narrow-to-message n)
      (- (point-max) (point-min)))))

(defun pcmail-message-line-count (n)
  "Return number of lines in message absolute-numbered N.
Args: (n)"
  (save-excursion
    (save-restriction
      (pcmail-narrow-to-message n)
      (count-lines (point-min) (point-max)))))

(defun pcmail-message-contents (n)
  "Return message N's contents
Args: (n)
  Returns contents of message absolute-numbered N, including all Babyl header 
and trailer information, as a string."
  (save-restriction
    (widen)
    (buffer-substring (pcmail-msgbeg n) (pcmail-msgend n))))

(defun pcmail-delete-field (f)
  "Delete all occurrences of header field F and its contents.
Args: (f)
  Go to the beginning of current narrowed region, search for field F, and
delete it and its contents.  Repeat the search until no more Fs are found.
Assume that the region is already narrowed to a message header.  Return 
number of Fs deleted if F exists, NIL else."
  (let ((nfields) (start))
    (goto-char (point-min))
    (while (re-search-forward (concat "^" (regexp-quote f) ":[ \t]*") nil t)
      (setq start (match-beginning 0))
      (if (null nfields) (setq nfields 1) (setq nfields (1+ nfields)))
      (while (progn (forward-line 1) (looking-at "[ \t]")))
      (delete-region start (point)))
    nfields))

(defun pcmail-narrow-to-field (f)
  "Narrow the current buffer to the contents of header field F.
Args: (f)
  Go to the beginning of current narrowed region, search for field F, and
narrow to its contents.  Assume that the region is already narrowed to a 
message header.  Return non-NIL if F exists, NIL else."
  (let ((start))
    (goto-char (point-min))
    (cond ((re-search-forward (concat "^" (regexp-quote f) ":[ \t]*")
			      nil t)
	   (setq start (point))
	   (while (progn (forward-line 1) (looking-at "[ \t]")))
	   (narrow-to-region start (point))
	   (goto-char start)
	   t))))

(defun pcmail-maybe-set-message-vectors ()
  "Reset message vectors if any are NIL.
Args: none"
  (or (and pcmail-total-messages
	   pcmail-current-subset-message
	   pcmail-attr-vector
	   pcmail-message-vector)
      (pcmail-set-message-vectors)))

(defun pcmail-set-message-vectors (&optional start)
  "Scan folder, setting up message information vectors.
Args: (&optional start)
  Set up current buffer's message information vectors.  Build current
subset using default filter name.  Deal with expired messages.  Message
scan begins at buffer position START, if present.  If start is not present,
flush old message counters before scan, otherwise append new information
to old counters.  See also pcmail-scan-folder-messages."
  (let ((total-messages 0)
	(i 0)
	(case-fold-search)
	(timely-list)
	(messages-list)
	(filter)
	(filter-start)
	(attr-list))
    (unwind-protect
	(progn
	  (cond ((null start)		;new?
		 (and (vectorp pcmail-message-vector)
		      (while (< i (length pcmail-message-vector))
			(move-marker (aref pcmail-message-vector i) nil)
			(setq i (1+ i))))
		 (setq pcmail-message-vector 
		       (make-vector 1
				    (save-restriction
				      (widen)
				      (point-min-marker)))
		       pcmail-current-subset-message 1
		       pcmail-attr-vector nil
		       pcmail-total-messages 0
		       pcmail-date-vector (make-vector 1 nil)
		       pcmail-priority-vector (make-vector 1 nil)
		       pcmail-summary-vector (make-vector 1 nil)
		       filter (pcmail-filter-description
			       pcmail-default-filter-name)))
		(t			;or append?
		 (setq filter-start (1+ pcmail-total-messages)
		       filter pcmail-current-filter-description)))
	  (pcmail-scan-folder-messages start))
      (setq pcmail-message-vector 
	    (vconcat pcmail-message-vector (apply 'vector messages-list))
	    pcmail-attr-vector
	    (vconcat pcmail-attr-vector (apply 'vector attr-list))
	    pcmail-date-vector
	    (vconcat pcmail-date-vector (make-vector total-messages nil))
	    pcmail-priority-vector
	    (vconcat pcmail-priority-vector (make-vector total-messages nil))
	    pcmail-summary-vector
	    (vconcat pcmail-summary-vector (make-vector total-messages nil))
	    pcmail-total-messages (+ pcmail-total-messages total-messages))
      (pcmail-build-subset-membership filter filter-start)
      (pcmail-hack-timely-messages timely-list)
      (and (>= total-messages pcmail-progress-interval)
	   (message "Counting messages in %s...done (%d message%s)" 
		    pcmail-folder-name total-messages 
		    (pcmail-s-ending total-messages))))))
	
(defun pcmail-msgbeg (n)
  "Return marker position of beginning of message absolute-numbered N.
Args: none"
  (aref pcmail-message-vector n))

(defun pcmail-msgend (n)
  "Return marker position of end of message absolute-numbered N.
Args: none"
  (aref pcmail-message-vector (1+ n)))

(provide 'pcmailmove)
