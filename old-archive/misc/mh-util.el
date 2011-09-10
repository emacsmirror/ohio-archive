;From utkcs2!emory!swrinde!ucsd!ucbvax!SOMEWHERE.BERKELEY.EDU!aks Tue Jun  5 08:30:50 EDT 1990
;Article 4362 of comp.emacs:
;Path: utkcs2!emory!swrinde!ucsd!ucbvax!SOMEWHERE.BERKELEY.EDU!aks
;>From: aks@SOMEWHERE.BERKELEY.EDU (Alan Stebbens)
;Newsgroups: comp.emacs
;Subject: Some nice utilities for mh-e.el
;Message-ID: <9006050614.AA05560@somewhere>
;Date: 5 Jun 90 06:14:02 GMT
;Sender: daemon@ucbvax.BERKELEY.EDU
;Lines: 431
;
;One of the most frequent things I do when reading mail via mh-e in Emacs
;is to delete a bunch of mail concerning the _same_ subject.  The next
;frequent thing is to view a piece of mail, and then wonder if someone
;else in my organization has answered it; that is, I want to know if
;there is a piece of mail with the same subject, or, thirdly, another
;reply by the same sender.  I often resort to using one of the builtin
;search commands, all of which, however require either proper prior
;postioning (how's _that_ for assonance) or manual input.
;
;Again, trying to make mh-e and Emacs Do The Right Thing, I've developed
;the following code which you may also find useful.  Included with it are
;some other mh-e utilities I've acquired from the net; if you recognize a
;function as being originally your own, please speak up, and I'll be glad
;to place your name in its proper place of honor.
;
;"mh-util.el" consists of the following goodies (currently :^):
;
;mh-next-msg-same-kind	N	[mh-folder-map]
;mh-prev-msg-same-kind	P	[mh-folder-map]
;	Functions to move to the next message of the same kind, either
;	by subject or by sender, depending upon either the variable
;	mh-search-kind-default, or the prefix argument.
;
;mh-toggle-kind-search	T	[mh-folder-map]
;	Allows you to easily switch between subject searching and sender
;	searching.
;
;mh-delete-by-subject	D	[mh-folder-map]
;	A function to delete messages in the current folder by subject
;
;mh-delete-by-body	M-d	[mh-folder-map]
;	A function to delete message by a body text pattern.
;
;mh-do-pick-delete	C-c C-d	[mh-pick-map]
;	An enhancement to M-x mh-pick-search, so that after invoking
;	mh-pick-search to construct a pick pattern, you can do C-c C-d
;	to cause the pattern to be used as a search-and-delete, instead
;	of just putting it into a sequence list.
;
;mh-toggle-headers	M-t 	[mh-folder-map]
;	A function to toggle MH headers
;
;and
;
;mh-next-pick-field
;	A "replacement" for the original mh-e function, which did not
;	like header values with embedded colons, like:
;
;	"Subject: Re: blah blah"
;
;	The new function has a modified regexp which doesn't mind the
;	embedded colons.
;
;To use this, place the following code in a file in your "load-path",
;called "mh-utils.el", (you may byte-compile it if you wish), and put 
;
;  (require 'mh-utils)
;
;in your ~/.emacs file.
;
;One thing you may notice: the next-msg-by-kind functions use only the
;folder scan listing to discover the message, while the delete-by-KIND
;functions use a pick-search.  There is no strong philosophical reason
;for this, except that the tools to do the multiple sequences, which is
;what you need for the multiple deletion task, were already set up to use
;pick sequences, while the searching by KIND was easier to write using
;the folder scan listing.  Feel free to comment.
;
;Enjoy
;
;Alan Stebbens        <aks@hub.ucsb.edu>             (805) 961-3221
;     Center for Computational Sciences and Engineering (CCSE)
;          University of California, Santa Barbara (UCSB)
;           3111 Engineering I, Santa Barbara, CA 93106
;
;============================= cut here ===================================
;; mh-util.el
;; $Header$
;;
;;  Copyright (C) 1990 Free Software Foundation, Inc.
;;
;;     Author:  Alan Stebbens <aks@hub.ucsb.edu>
;;	Please send suggestions and corrections to the above address.
;;
;;; GNU Emacs is distributed in the hope that it will be useful, but
;;; without any warranty.  No author or distributor accepts
;;; responsibility to anyone for the consequences of using it or for
;;; whether it serves any particular purpose or works at all, unless he
;;; says so in writing.

;;; Everyone is granted permission to copy, modify and redistribute GNU
;;; Emacs, but only under the conditions described in the document "GNU
;;; Emacs copying permission notice".  An exact copy of the document is
;;; supposed to have been given to you along with GNU Emacs so that you
;;; can know how you may redistribute it all.  It should be in a file
;;; named COPYING.  Among other things, the copyright notice and this
;;; notice must be preserved on all copies.
;;
;; This file contains enhancements to the mh-e package, a GNU Emacs 
;; front end to the MH mail system, providing:
;;
;;    o  mh-toggle-headers     - show all/default headers
;;    o  mh-next-msg-same-kind - next message with the same {sender,subject}
;;    o  mh-prev-msg-same-kind - prev message with the same {sender,subject}
;;    o  mh-delete-by-subject  - delete messages with given subject
;;    o  mh-delete-by-body     - delete messages with given body text
;;    o  mh-do-pick-delete     - delete messages matching pick pattern
;;
;; See the end of this file for the new key bindings.
;;
;; This file also fixes a bug in mh-next-pick-field, which refuses to allow
;; colons in the value of any component.
;;
;; Last Edited:
;; 
;; Mon Jun  4 21:16:26 1990 by Alan Stebbens (aks at somewhere.ucsb.edu)
;; 	 Added mh-{next,prev,find}-same-{kind,sender,subject},
;;	 and mh-match-msg.
;;	 Made mh-do-pick-delete call mh-next-msg at end.
;; 
;; Fri May 25 11:19:06 1990 by Alan Stebbens (aks at somewhere.ucsb.edu)
;; 	 Included mh-next-pick-field to fix bug (until fixed in
;; 	 mh-e.el).
;; 
;; Fri May 25 10:22:39 1990 by Alan Stebbens (aks at somewhere.ucsb.edu)
;; 	 Initial version.
;;

(require 'mh-e)				; be sure mh-e is loaded first


;; mh-toggle-headers
;;
;; Display or not, all visible headers

(defun mh-toggle-headers (arg) "\
Set display of all message headers according to ARG: if nil, toggle the
current value; if 0, reset to default value (nil); if 1, show all
message headers."
  (interactive "p")
  (setq mh-visible-headers (cond ((or (eq 4 arg) 
				      (and (eq 1 arg)
					   (not mh-visible-headers)))
				  ".*")))
  (save-excursion
    (if (get-buffer mh-show-buffer)
	(kill-buffer mh-show-buffer)))
  (mh-show (mh-get-msg-num t)))



(defvar mh-kind-search-default 'subject "\
*This variable should be set to either 'subject or 'sender, indicating the
default kind of search when used with \\[mh-next-msg-same-kind] or
\\[mh-prev-msg-same-kind].")

;; The following constants tailor the location in the folder scan listing of
;; the subject and body.  If you ever change the format of the scan listing
;; produced by mh-scan-folder, then you may have to alter these constants also.

(defconst mh-cur-sender-offset 12
  "Offset in the current folder scan where the sender name begins.")

(defconst mh-cur-subject-offset 31
  "Offset in the current folder scan where the subject begins.")

(defconst mh-cur-scan-sender-regexp
  (concat "^"   (make-string mh-cur-sender-offset ?\. )
	  "\\(" (make-string (- mh-cur-subject-offset mh-cur-sender-offset) ?\. )
	  "\\)")
  "Regexp to match the sender portion of the current message.")

(defconst mh-cur-scan-subject-regexp 
  (concat "^" (make-string mh-cur-subject-offset ?\. )
	  "\\(\\([^<\n]\\|<[^<\n]\\)+\\)\\(<<\\|$\\)")
  "Regexp which matches the subject of the current scan line.")

(defconst mh-good-sender-regexp 
  (concat mh-good-msg-regexp
	  (make-string (- mh-cur-sender-offset (1+ mh-cmd-note)) ?\. ))
  "Regexp used to match good message up to the sender portion.")

(defconst mh-good-subject-regexp
  (concat mh-good-msg-regexp
	  (make-string (- mh-cur-subject-offset (1+ mh-cmd-note)) ?\. ))
  "Regexp matching a good message up to the subject portion.")

(defconst mh-cur-scan-body-regexp "<<[ \t]*\\(.*\\)"
  "Regexp which matches the body included as part of the current scan line.")

(defconst mh-delete-body-prompt-length 50
  "Default length of the initial substring prompt for \\[mh-delete-by-body]")

;; mh-next-msg-same

(defun mh-next-msg-same-kind (arg) "\
Search forward in the current folder for another message with the same
subject, as the current message.  If prefix ARG is given, search by sender
name rather than subject."
  (interactive "P")
  (funcall (mh-same-kind-func arg) 'forward))

;; mh-prev-same-subject

(defun mh-prev-msg-same-kind (arg) "\
Search backward in the current folder for another message with the same
subject as the current message.  If prefix ARG is given, search by sender
name rather than subject."
  (interactive "P")
  (funcall (mh-same-kind-func arg) 'backward))

;; mh-same-kind-func

(defun mh-same-kind-func (arg) 
  (symbol-function 
   (intern (concat "mh-find-msg-same-" 
		   (if arg 
		       (if (eq mh-kind-search-default 'sender) "subject" "sender")
		     (if (eq mh-kind-search-default 'sender) "sender" "subject"))))))

(defun mh-toggle-kind-search () "\
Toggle the kind of message search between 'subject and 'sender."
  (interactive)
  (setq mh-kind-search-default 
	(if (eq mh-kind-search-default 'sender) 'subject 'sender))
  (message "Next default searches will be %s kind" (symbol-name mh-kind-search-default)))

;; mh-find-same-subject

(defun mh-find-msg-same-subject (direction) "\
Search according to DIRECTION ('forward or 'backward) for another message
with the same subject as the current message."
  (let* ((subject (save-excursion
		    (beginning-of-line)
		    (looking-at mh-cur-scan-subject-regexp)
		    (buffer-substring (match-beginning 1) (match-end 1))))
	 (regexp (concat mh-good-subject-regexp
			 (regexp-quote subject))))
    (mh-match-msg direction regexp "subject" subject)))

;; mh-find-msg-same-sender

(defun mh-find-msg-same-sender (direction) "\
Search the current folder given DIRECTION for another message with the
same sender."
  (let ((sender (save-excursion
		  (beginning-of-line)
		  (looking-at mh-cur-scan-sender-regexp)
		  (buffer-substring (match-beginning 1) (match-end 1)))))
    (if (string-match "\\`\\(.+[^ \t]\\)[ \t]+\\'" sender)
	(setq sender (substring sender 0 (match-end 1))))
    (setq regexp (concat mh-good-sender-regexp (regexp-quote sender)))
    (mh-match-msg direction regexp "sender" sender)))

;; mh-match-msg

(defun mh-match-msg (direction regexp component value) "\
Find the next message given DIRECTION matching the given REGEXP.
Third and fourth args are COMPONENT and VALUE, for error messages
on search failure."
  (let* ((dir (symbol-name direction))
	 (search (intern (concat "re-search-" dir)))
	 (adjust-line (intern (concat dir "-line")))
	 (msg (save-excursion
		(beginning-of-line (if (eq direction 'forward) 2 0))
		(if (funcall search regexp nil t)
		    (mh-get-msg-num nil)))))
    (if (and msg (mh-goto-msg msg))
	(setq mh-next-direction direction)
      (message "No more messages %s with %s: \"%s\"" dir component value)
      (ding))))

;; mh-delete-by-subject
;; Delete the current msg and any others with the same subject.

(defun mh-delete-by-subject (subject) "\
Search the current folder for messages with the given SUBJECT and delete
them."
  (interactive (list (read-string "Delete by subject: " (mh-current-subject))))
  (let ((folder mh-current-folder))
    (set-buffer (get-buffer-create " delete-pattern"))
    (mh-make-pick-template)
    (mh-insert-fields "Subject:" subject)
    (setq mh-searching-folder folder)
    (mh-do-pick-delete)))

;; mh-delete-by-body
;; Delete any messages with the same body text as the current message.

(defun mh-delete-by-body (body) "\
Search the current folder for messages with the given BODY and delete them.
Prompt for BODY if not supplied."
  (interactive (list (read-string "Delete by text: " (mh-current-body))))
  (let ((folder mh-current-folder))
    (set-buffer (get-buffer-create " delete-pattern"))
    (mh-make-pick-template)
    (mh-goto-header-end 1)
    (insert body)
    (setq mh-searching-folder folder)
    (mh-do-pick-delete)))

;; mh-current-subject

(defun mh-current-subject ()
  "Get the current subject, either from the folder scan listing, or from
the current message, if it is being shown."
  (if mh-showing
      (save-window-excursion
	(set-buffer mh-show-buffer)
	(save-excursion
	  (mh-get-field "Subject:")))
    (save-excursion
      (beginning-of-line)
      (if (looking-at mh-cur-scan-subject-regexp)
	  (buffer-substring (match-beginning 1) (match-end 1))))))

;; mh-current-body

(defun mh-current-body ()
  "Get the current message body, either from the folder scan listing, or
from the current message, if it is being shown."
  (let ((str (if mh-showing 
		 (save-window-excursion
		   (set-buffer mh-show-buffer)
		   (save-excursion
		     (if (and (mh-goto-header-end 1)
			      (re-search-forward "[^ \t\n]" nil t))
			 (let (beg end)
			   (setq beg (1- (point)))
			   (end-of-line)
			   (buffer-substring beg (point))))))
	       (save-excursion 
		 (let (end)
		   (end-of-line)
		   (setq end (point))
		   (beginning-of-line)
		   (if (re-search-forward mh-cur-scan-body-regexp end t)
		       (buffer-substring (match-beginning 1) (match-end 1))))))))
    (if (and str (> (length str) mh-delete-body-prompt-length))
	(substring str 0 mh-delete-body-prompt-length)
      str)))
    
;; mh-do-pick-delete
;;
;; Take the current pick-pattern buffer and use it to search and match messages
;; to delete.
;; (copied from mh-do-pick-search, but we use a different sequence name: 'delete)

(defun mh-do-pick-delete ()
  "Find messages that match the qualifications in the current pattern buffer.
Messages are searched for in the folder named in mh-searching-folder.
Delete messages found."
  (interactive)
  (let ((pattern-buffer (buffer-name))
	(searching-buffer mh-searching-folder)
	(range)
	(pattern nil)
	(new-buffer nil))
    (save-excursion
      (cond ((get-buffer searching-buffer)
	     (set-buffer searching-buffer)
	     (setq range (format "%d-%d" mh-first-msg-num mh-last-msg-num)))
	    (t
	     (mh-make-folder searching-buffer)
	     (setq range "all")
	     (setq new-buffer t))))
    (message "Searching %s..." searching-buffer)
    (goto-char (point-min))
    (while (setq pattern (mh-next-pick-field pattern-buffer))
      (setq msgs (mh-seq-from-command searching-buffer
				      'delete
				      (nconc (cons "pick" pattern)
					     (list searching-buffer
						   range
						   "-sequence" "delete"
						   "-list"))))
      (setq range "delete"))
    (message "Searching %s...done" searching-buffer)
    (if new-buffer
	(mh-scan-folder searching-buffer msgs)
	(switch-to-buffer searching-buffer))
    (delete-other-windows)
    (message "Deleting %d messages..." (length msgs))
    (mh-delete-msg-no-motion 'delete)
    (message "Deleted %d messages." (length msgs))
    (mh-next-msg)))

;; Make new commands available by keystroke

(define-key mh-folder-mode-map "D"         'mh-delete-by-subject)
(define-key mh-folder-mode-map "\M-d"      'mh-delete-by-body)
(define-key mh-folder-mode-map "\M-t"	   'mh-toggle-headers)
(define-key mh-folder-mode-map "N"	   'mh-next-msg-same-kind)
(define-key mh-folder-mode-map "P"	   'mh-prev-msg-same-kind)
(define-key mh-folder-mode-map "T"	   'mh-toggle-kind-search)

(define-key mh-pick-mode-map   "\C-c\C-d"  'mh-do-pick-delete)


;;
;; Fixes the bug in the standard mh-e.el (modified the regexp).
;;

(defun mh-next-pick-field (buffer)
  ;; Return the next piece of a pick argument that can be extracted from the
  ;; BUFFER.  Returns nil if no pieces remain.
  (set-buffer buffer)
  (let ((case-fold-search t))
    (cond ((eobp)
	   nil)
	  ((re-search-forward "^\\([a-z][^:\n \t]*\\):[ \t]*\\([a-z0-9].*\\)$" nil t)
	   (let* ((component
		   (format "-%s"
			   (downcase (buffer-substring (match-beginning 1)
						       (match-end 1)))))
		  (pat (buffer-substring (match-beginning 2) (match-end 2))))
	       (forward-line 1)
	       (list component pat)))
	  ((re-search-forward "^-*$" nil t)
	   (forward-char 1)
	   (let ((body (buffer-substring (point) (point-max))))
	     (if (and (> (length body) 0) (not (equal body "\n")))
		 (list "-search" body)
		 nil)))
	  (t
	   nil))))


;; Announce that we're loaded

(provide 'mh-util)


