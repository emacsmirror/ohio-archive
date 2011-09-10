From ukma!uflorida!novavax!weiner@RUTGERS.EDU Fri May 19 14:36:39 1989
To: unix-emacs@bbn.com
Date: 28 Apr 89 06:20:18 GMT
From: Bob Weiner <ukma!uflorida!novavax!weiner@RUTGERS.EDU>
Sender: arpa-unix-emacs-request@bbn.com
Subject: rmailsum.el, improved, see hdr, also get posting of rmail.el
Organization: Nova University, Fort Lauderdale, FL
Source-Info:  From (or Sender) name not authenticated.

;; "RMAIL" mail reader for Emacs.
;; Copyright (C) 1985 Free Software Foundation, Inc.

;; Bob Weiner, Motorola, Inc., 3/30/89
;;  Modified 'rmail-summary-next-msg to move to the next (on first non-del
;;  message) or prev (on last non-del message) non-del message unless second
;;  optional argument 'no-reverse' is non-nil or current message is not deleted.
;;  Added documentation to this command.
;;  This change combined with those in rmail.el means the user normally,
;;  never sees a message once he has deleted it.
;;
;;  Defined new command 'rmail-summary-get-new-mail' bound to {g}.
;;  Added new command 'rmail-summary-last-message' bound to {>}.
;;
;; Bob Weiner, Motorola, Inc., 4/7/89
;;  Added 'rmail-summary-mark-deleted' function; modified
;;  'rmail-summary-delete-forward' to call this added function.
;;
;;  Defined new command 'rmail-summary-edit-current-message' bound to {e}; also
;;  defined commands 'rmail-summary-cease-edit' and 'rmail-summary-abort-edit'
;;  to support its functionality.
;;  Defined new command 'rmail-summary-beginning-of-message' bound to {.}.
;;  Defined new command 'rmail-summary-mail' bound to {m}.
;;  Defined new command 'rmail-summary-continue' bound to {c}.
;;  Defined new command 'rmail-summary-reply' bound to {r} and command
;;  'rmail-summary-reply-send-and-exit' to support its functionality.
;;  Defined new command 'rmail-summary-forward' bound to {f}.
;;  Defined new command 'rmail-summary-expunge' bound to {x}.
;;  Moved key binding of 'rmail-summary-exit' from {x} to {w}.  Since almost
;;  all Rmail functions can now be done from the summary buffer there is little
;;  need for this exit coomand, but it is like 'wiping' the buffer away.
;;
;;  Added 3rd optional argument SKIP-RMAIL to 'rmail-summary-goto-msg' to say
;;  don't update RMAIL buffer display; used by functions in rmail.el.
;;
;; Bob Weiner, Motorola, Inc., 4/11/89
;;  Added 'rmail-summary-delete-backward' bound to {C-d}.  Added prefix arg to
;;  'rmail-summary-delete-forward' so it matches 'rmail-delete-forward'.
;;  Added 'rmail-summary-expunge-and-save' bound to {s}.
;;
;; Bob Weiner, Motorola, Inc., 4/13/89
;;  Changed description text in 'rmail-summary-mode' to reflect new
;;  functionality.
;;  Added 'rmail-summary-undelete-many' command bound to {M-u}.  Also executed
;;  when 'rmail-summary-undelete' is called with a prefix argument.
;;  Added 'rmail-summary-deleted-p' predicate for testing if rmail-summary
;;  buffer indicates a message is deleted.


;; This file is part of GNU Emacs.

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


;; summary things

(defun rmail-summary ()
  "Display a summary of all messages, one line per message."
  (interactive)
  (rmail-new-summary "All" nil))

(defun rmail-summary-by-labels (labels)
  "Display a summary of all messages with one or more LABELS.
LABELS should be a string containing the desired labels, separated by commas."
  (interactive "sLabels to summarize by: ")
  (if (string= labels "")
      (setq labels (or rmail-last-multi-labels
		       (error "No label specified"))))
  (setq rmail-last-multi-labels labels)
  (rmail-new-summary (concat "labels " labels)
		     'rmail-message-labels-p
		     (concat ", \\(" (mail-comma-list-regexp labels) "\\),")))

(defun rmail-summary-by-recipients (recipients &optional primary-only)
  "Display a summary of all messages with the given RECIPIENTS.
Normally checks the To, From and Cc fields of headers;
but if PRIMARY-ONLY is non-nil (prefix arg given),
 only look in the To and From fields.
RECIPIENTS is a string of names separated by commas."
  (interactive "sRecipients to summarize by: \nP")
  (rmail-new-summary
   (concat "recipients " recipients)
   'rmail-message-recipients-p
   (mail-comma-list-regexp recipients) primary-only))

(defun rmail-message-recipients-p (msg recipients &optional primary-only)
  (save-restriction
    (goto-char (rmail-msgbeg msg))
    (search-forward "\n*** EOOH ***\n")
    (narrow-to-region (point) (progn (search-forward "\n\n") (point)))
    (or (string-match recipients (or (mail-fetch-field "To") ""))
	(string-match recipients (or (mail-fetch-field "From") ""))
	(if (not primary-only)
	    (string-match recipients (or (mail-fetch-field "Cc") ""))))))

(defun rmail-new-summary (description function &rest args)
  "Create a summary of selected messages.
DESCRIPTION makes part of the mode line of the summary buffer.
For each message, FUNCTION is applied to the message number and ARGS...
and if the result is non-nil, that message is included.
nil for FUNCTION means all messages."
  (message "Computing summary lines...")
  (or (and rmail-summary-buffer
	   (buffer-name rmail-summary-buffer))
      (setq rmail-summary-buffer
	    (generate-new-buffer (concat (buffer-name) "-summary"))))
  (let ((summary-msgs ())
	(new-summary-line-count 0))
    (let ((msgnum 1)
	  (buffer-read-only nil))
      (save-restriction
	(save-excursion
	  (widen)
	  (goto-char (point-min))
	  (while (>= rmail-total-messages msgnum)
	    (if (or (null function)
		    (apply function (cons msgnum args)))
		(setq summary-msgs
		      (cons (rmail-make-summary-line msgnum)
			    summary-msgs)))
	    (setq msgnum (1+ msgnum))))))
    (let ((sbuf rmail-summary-buffer)
	  (rbuf (current-buffer))
	  (total rmail-total-messages)
	  (mesg rmail-current-message))
      (pop-to-buffer sbuf)
      (let ((buffer-read-only nil))
	(erase-buffer)
	(cond (summary-msgs
	       (princ (nreverse summary-msgs) sbuf)
	       (delete-char -1)
	       (subst-char-in-region 1 2 ?\( ?\ ))))
      (setq buffer-read-only t)
      (goto-char (point-min))
      (rmail-summary-mode)
      (make-local-variable 'minor-mode-alist)
      (setq minor-mode-alist (list ": " description))
      (setq rmail-buffer rbuf
	    rmail-total-messages total)
      (rmail-summary-goto-msg mesg t)))
  (message "Computing summary lines...done"))

(defun rmail-make-summary-line (msg)
  (let ((line (or (aref rmail-summary-vector (1- msg))
		  (progn
		    (setq new-summary-line-count
			  (1+ new-summary-line-count))
		    (if (zerop (% new-summary-line-count 10))
			(message "Computing summary lines...%d"
				 new-summary-line-count))
		    (rmail-make-summary-line-1 msg)))))
    ;; Fix up the part of the summary that says "deleted" or "unseen".
    (aset line 4
	  (if (rmail-message-deleted-p msg) ?\D
	    (if (= ?0 (char-after (+ 3 (rmail-msgbeg msg))))
		?\- ?\ )))
    line))

(defun rmail-make-summary-line-1 (msg)
  (goto-char (rmail-msgbeg msg))
  (let* ((lim (save-excursion (forward-line 2) (point)))
	 pos
	 (labels
	  (progn
	    (forward-char 3)
	    (concat
;	     (if (save-excursion (re-search-forward ",answered," lim t))
;		 "*" "")
;	     (if (save-excursion (re-search-forward ",filed," lim t))
;		 "!" "")
	     (if (progn (search-forward ",,") (eolp))
		 ""
	       (concat "{"
		       (buffer-substring (point)
					 (progn (end-of-line) (point)))
		       "} ")))))
	 (line
	  (progn
	    (forward-line 1)
	    (if (looking-at "Summary-line: ")
		(progn
		  (goto-char (match-end 0))
		  (setq line
			(buffer-substring (point)
					  (progn (forward-line 1) (point)))))))))
    ;; Obsolete status lines lacking a # should be flushed.
    (and line
	 (not (string-match "#" line))
	 (progn
	   (delete-region (point)
			  (progn (forward-line -1) (point)))
	   (setq line nil)))
    ;; If we didn't get a valid status line from the message,
    ;; make a new one and put it in the message.
    (or line
	(let* ((case-fold-search t)
	       (next (rmail-msgend msg))
	       (beg (if (progn (goto-char (rmail-msgbeg msg))
			       (search-forward "\n*** EOOH ***\n" next t))
			(point)
		      (forward-line 1)
		      (point)))
	       (end (progn (search-forward "\n\n" nil t) (point))))
	  (save-restriction
	    (narrow-to-region beg end)
	    (goto-char beg)
	    (setq line (rmail-make-basic-summary-line)))
	  (goto-char (rmail-msgbeg msg))
	  (forward-line 2)
	  (insert "Summary-line: " line)))
    (setq pos (string-match "#" line))
    (aset rmail-summary-vector (1- msg)
	  (concat (format "%4d  " msg)
		  (substring line 0 pos)
		  labels
		  (substring line (1+ pos))))))

(defun rmail-make-basic-summary-line ()
  (goto-char (point-min))
  (concat (save-excursion
	    (if (not (re-search-forward "^Date:" nil t))
		"      "
	      (cond ((re-search-forward "\\([^0-9:]\\)\\([0-3]?[0-9]\\)\\([- \t_]+\\)\\([adfjmnos][aceopu][bcglnprtvy]\\)"
		      (save-excursion (end-of-line) (point)) t)
		     (format "%2d-%3s"
			     (string-to-int (buffer-substring
					     (match-beginning 2)
					     (match-end 2)))
			     (buffer-substring
			      (match-beginning 4) (match-end 4))))
		    ((re-search-forward "\\([^a-z]\\)\\([adfjmnos][acepou][bcglnprtvy]\\)\\([-a-z \t_]*\\)\\([0-9][0-9]?\\)"
		      (save-excursion (end-of-line) (point)) t)
		     (format "%2d-%3s"
			     (string-to-int (buffer-substring
					     (match-beginning 4)
					     (match-end 4)))
			     (buffer-substring
			      (match-beginning 2) (match-end 2))))
		    (t "??????"))))
	  "  "
	  (save-excursion
	    (if (not (re-search-forward "^From:[ \t]*" nil t))
		"                         "
	      (let* ((from (mail-strip-quoted-names
			    (buffer-substring
			     (1- (point))
			     (progn (end-of-line)
				    (skip-chars-backward " \t")
				    (point)))))
		     len mch lo)
		(if (string-match (concat "^"
					  (regexp-quote (user-login-name))
					  "\\($\\|@\\)")
				  from)
		    (save-excursion
		      (goto-char (point-min))
		      (if (not (re-search-forward "^To:[ \t]*" nil t))
			  nil
			(setq from
			      (concat "to: "
				      (mail-strip-quoted-names
				       (buffer-substring
					(point)
					(progn (end-of-line)
					       (skip-chars-backward " \t")
					       (point)))))))))
		(setq len (length from))
		(setq mch (string-match "[@%]" from))
		(format "%25s"
			(if (or (not mch) (<= len 25))
			    (substring from (max 0 (- len 25)))
			  (substring from
				     (setq lo (cond ((< (- mch 9) 0) 0)
						    ((< len (+ mch 16))
						     (- len 25))
						    (t (- mch 9))))
				     (min len (+ lo 25))))))))
	  "  #"
	  (if (re-search-forward "^Subject:" nil t)
	      (progn (skip-chars-forward " \t")
		     (buffer-substring (point)
				       (progn (end-of-line)
					      (point))))
	    (re-search-forward "[\n][\n]+" nil t)
	    (buffer-substring (point) (progn (end-of-line) (point))))
	  "\n"))

(defun rmail-summary-next-all (&optional number)
  (interactive "p")
  (forward-line (if number number 1))
  (rmail-summary-goto-msg))

(defun rmail-summary-previous-all (&optional number)
  (interactive "p")
  (forward-line (- (if number number 1)))
  (rmail-summary-goto-msg))

(defun rmail-summary-next-msg (&optional number no-reverse)
"Display next non-deleted msg from rmail file.
With optional prefix argument NUMBER, moves forward this number of non-deleted
messages or backward if number is negative.  If no non-deleted message is found
and the current message is deleted, the next non-deleted message in the
opposite direction is found regardless of the value of number, unless optional
second argument NO-REVERSE is non-nil."
  (interactive "p")
  (forward-line 0)
  (and (> number 0) (forward-line 1))
  (let ((count (if (< number 0) (- number) number))
	(search (if (> number 0) 're-search-forward 're-search-backward))
	(non-del-msg-found nil))
    (while (and (> count 0) (setq non-del-msg-found
				  (or (funcall search "^.....[^D]" nil t)
				      non-del-msg-found)))
      (setq count (1- count)))
    (if (or no-reverse non-del-msg-found (looking-at "^.....[^D]"))
	(rmail-summary-goto-msg)
      (rmail-summary-next-msg (if (> number 0) -1 1) t))))

(defun rmail-summary-previous-msg (&optional number)
  (interactive "p")
  (rmail-summary-next-msg (- (if number number 1))))

(defun rmail-summary-delete-forward (&optional backward)
  "Delete this message and move to next nondeleted one.
Deleted messages stay in the file until the \\[rmail-expunge] command is given.
With prefix argument, delete and move backward."
  (interactive "P")
  (let (end)
    (rmail-summary-goto-msg)
    (pop-to-buffer rmail-buffer)
    (rmail-delete-forward backward)
    (pop-to-buffer rmail-summary-buffer)))

(defun rmail-summary-delete-backward ()
  "Delete this message and move to previous nondeleted one.
Deleted messages stay in the file until the \\[rmail-expunge] command is given."
  (interactive)
  (rmail-summary-delete-forward t))

(defun rmail-summary-mark-deleted (&optional n undel)
  (and n (goto-line n) (beginning-of-line))
  (let ((buffer-read-only nil))
    (skip-chars-forward " ")
    (skip-chars-forward "[0-9]")
    (if undel
	(if (looking-at "D")
	    (progn (delete-char 1) (insert " ")))
      (delete-char 1)
      (insert "D")))
  (beginning-of-line))

(defun rmail-summary-mark-undeleted (n)
  (rmail-summary-mark-deleted n t))

(defun rmail-summary-deleted-p (&optional n)
  (save-excursion
    (and n (goto-line n) (beginning-of-line))
    (skip-chars-forward " ")
    (skip-chars-forward "[0-9]")
    (looking-at "D")))

(defun rmail-summary-undelete (&optional arg)
  "Undelete current message.  Optional prefix ARG means undelete ARG previous
messages."
  (interactive "P")
  (if arg
      (rmail-summary-undelete-many arg)
    (let ((buffer-read-only nil))
      (end-of-line)
      (cond ((re-search-backward "\\(^ *[0-9]*\\)\\(D\\)" nil t)
	     (replace-match "\\1 ")
	     (rmail-summary-goto-msg)
	     (pop-to-buffer rmail-buffer)
	     (and (rmail-message-deleted-p rmail-current-message)
		  (rmail-undelete-previous-message))
	     (pop-to-buffer rmail-summary-buffer))
	    (t
	      (rmail-summary-goto-msg))))))

(defun rmail-summary-undelete-many (&optional n)
  "Undelete all deleted messages, optional argument N means undelete N previous
messages."
  (interactive "P")
  (set-buffer rmail-buffer)
  (let* ((init-msg (if n rmail-current-message rmail-total-messages))
	 (rmail-current-message init-msg)
	 (n (or n rmail-total-messages))
	 (msgs-undeled 0))
    (while (and (> rmail-current-message 0)
		(< msgs-undeled n))
      (if (rmail-message-deleted-p rmail-current-message)
	  (progn (rmail-set-attribute "deleted" nil)
		 (setq msgs-undeled (1+ msgs-undeled))))
      (setq rmail-current-message (1- rmail-current-message)))
    (set-buffer rmail-summary-buffer)
    (setq rmail-current-message init-msg msgs-undeled 0)
    (while (and (> rmail-current-message 0)
		(< msgs-undeled n))
      (if (rmail-summary-deleted-p rmail-current-message)
	  (progn (rmail-summary-mark-undeleted rmail-current-message)
		 (setq msgs-undeled (1+ msgs-undeled))))
      (setq rmail-current-message (1- rmail-current-message))))
  (rmail-summary-goto-msg))

;; Rmail Summary mode is suitable only for specially formatted data.
(put 'rmail-summary-mode 'mode-class 'special)

(defun rmail-summary-mode ()
  "Major mode in effect in Rmail summary buffer.
A subset of the Rmail mode commands are supported in this mode. 
As commands are issued in the summary buffer the corresponding
mail message is displayed in the rmail buffer.

n       Move to next undeleted message, or arg messages.
p       Move to previous undeleted message, or arg messages.
C-n	Move to next, or forward arg messages.
C-p	Move to previous, or previous arg messages.
>	Move to the last message in Rmail file.
j       Jump to the message at the cursor location.
g       Get new mail and recompute summary headers.
d       Delete the message at the cursor location and move to next message.
C-d	Delete this message, move to previous nondeleted.
u	Undelete this or prefix arg previous deleted messages.
M-u	Undelete all or prefix arg deleted messages.
s	Expunge and save the Rmail file.
q	Quit Rmail.
g	Move new mail from system spool directory or mbox into this file.
m	Mail a message.
c	Continue composing outgoing message started before.
r	Reply to this message.  Like m but initializes some fields.
f	Forward this message to another user.
e	Edit the current message.  C-c C-c to return to Rmail-summary.
w	Exit and kill the summary window.
x	Expunge deleted messages.
space   Scroll message in other window forward.
delete  Scroll message backward.

Entering this mode calls value of hook variable rmail-summary-mode-hook."
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'rmail-buffer)
  (make-local-variable 'rmail-total-messages)
  (setq major-mode 'rmail-summary-mode)
  (setq mode-name "RMAIL Summary")
  (use-local-map rmail-summary-mode-map)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (set-syntax-table text-mode-syntax-table)
  (run-hooks 'rmail-summary-mode-hook))

(defun rmail-summary-goto-msg (&optional n nowarn skip-rmail)
  (interactive "P")
  (if (consp n) (setq n (prefix-numeric-value n)))
  (if (eobp) (forward-line -1))
  (beginning-of-line)
  (let ((buf rmail-buffer)
	(cur (point))
	(curmsg (string-to-int
		 (buffer-substring (point)
				   (min (point-max) (+ 5 (point)))))))
    (if (not n)
	(setq n curmsg)
      (if (< n 1)
	  (progn (message "No preceding message")
		 (setq n 1)))
      (if (> n rmail-total-messages)
	  (progn (message "No following message")
		 (goto-char (point-max))
		 (rmail-summary-goto-msg)))
      (goto-char (point-min))
      (if (not (re-search-forward (concat "^ *" (int-to-string n)) nil t))
	  (progn (or nowarn (message "Message %d not found" n))
		 (setq n curmsg)
		 (goto-char cur))))
    (beginning-of-line)
    (skip-chars-forward " ")
    (skip-chars-forward "0-9")
    (save-excursion (if (= (following-char) ?-)
			(let ((buffer-read-only nil))
			  (delete-char 1)
			  (insert " "))))
    (beginning-of-line)
    (if skip-rmail
	nil
      (pop-to-buffer buf)
      (rmail-show-message n)
      (pop-to-buffer rmail-summary-buffer))))

(defvar rmail-summary-mode-map nil)

(if rmail-summary-mode-map
    nil
  (setq rmail-summary-mode-map (make-keymap))
  (suppress-keymap rmail-summary-mode-map)
  (define-key rmail-summary-mode-map "j" 'rmail-summary-goto-msg)
  (define-key rmail-summary-mode-map "n" 'rmail-summary-next-msg)
  (define-key rmail-summary-mode-map "p" 'rmail-summary-previous-msg)
  (define-key rmail-summary-mode-map "\C-n" 'rmail-summary-next-all)
  (define-key rmail-summary-mode-map "\C-p" 'rmail-summary-previous-all)
  (define-key rmail-summary-mode-map " " 'rmail-summary-scroll-msg-up)
  (define-key rmail-summary-mode-map "." 'rmail-summary-beginning-of-message)
  (define-key rmail-summary-mode-map "q" 'rmail-summary-quit)
  (define-key rmail-summary-mode-map "u" 'rmail-summary-undelete)
  (define-key rmail-summary-mode-map "\M-u" 'rmail-summary-undelete-many)
  (define-key rmail-summary-mode-map "e" 'rmail-summary-edit-current-message)
  (define-key rmail-summary-mode-map "w" 'rmail-summary-exit)
  (define-key rmail-summary-mode-map "x" 'rmail-summary-expunge)
  (define-key rmail-summary-mode-map "s" 'rmail-summary-expunge-and-save)
  (define-key rmail-summary-mode-map "g" 'rmail-summary-get-new-mail)
  (define-key rmail-summary-mode-map ">" 'rmail-summary-last-message)
  (define-key rmail-summary-mode-map "\177" 'rmail-summary-scroll-msg-down)
  (define-key rmail-summary-mode-map "d" 'rmail-summary-delete-forward)
  (define-key rmail-summary-mode-map "\C-d" 'rmail-summary-delete-backward)
  (define-key rmail-summary-mode-map "m" 'rmail-summary-mail)
  (define-key rmail-summary-mode-map "c" 'rmail-summary-continue)
  (define-key rmail-summary-mode-map "r" 'rmail-summary-reply)
  (define-key rmail-summary-mode-map "f" 'rmail-summary-forward)
  )

(defun rmail-summary-scroll-msg-up (&optional dist)
  "Scroll other window forward."
  (interactive "P")
  (scroll-other-window dist))

(defun rmail-summary-scroll-msg-down (&optional dist)
  "Scroll other window backward."
  (interactive "P")
  (scroll-other-window
   (cond ((eq dist '-) nil)
	 ((null dist) '-)
	 (t (- (prefix-numeric-value dist))))))

(defun rmail-summary-beginning-of-message ()
  "Show current message from the beginning."
  (interactive)
  (pop-to-buffer rmail-buffer)
  (beginning-of-buffer)
  (pop-to-buffer rmail-summary-buffer))

(defun rmail-summary-quit ()
  "Quit out of rmail and rmail summary."
  (interactive)
  (rmail-summary-exit)
  (rmail-quit))

(defun rmail-summary-exit ()
  "Exit rmail summary, remaining within rmail."
  (interactive)
  (bury-buffer (current-buffer))
  (if (get-buffer-window rmail-buffer)
      ;; Select the window with rmail in it, then delete this window.
      (select-window (prog1
			 (get-buffer-window rmail-buffer)
		       (delete-window (selected-window))))
    ;; Switch to the rmail buffer in this window.
    (switch-to-buffer rmail-buffer)))

(defun rmail-summary-expunge ()
  "Actually erase all deleted messages and recompute summary headers."
  (interactive)
  (set-buffer rmail-buffer)
  (rmail-only-expunge)
  (rmail-summary))

(defun rmail-summary-get-new-mail ()
  "Get new mail and recompute summary headers."
  (interactive)
  (set-buffer rmail-buffer)
  (rmail-get-new-mail)
  (rmail-summary))

(defun rmail-summary-last-message ()
  "Show last message in rmail file from summary buffer."
  (interactive)
  (rmail-summary-goto-msg rmail-total-messages))
  
(defun rmail-summary-edit-current-message ()
  "Edit the contents of this message."
  (interactive)
  (pop-to-buffer rmail-buffer)
  (rmail-edit-current-message)
  (define-key rmail-edit-map "\C-c\C-c" 'rmail-summary-cease-edit)
  (define-key rmail-edit-map "\C-c\C-]" 'rmail-summary-abort-edit))

(defun rmail-summary-cease-edit ()
  "Finish editing message. Switch back to Rmail proper and then to Rmail
summary buffer."
  (interactive)
  (define-key rmail-edit-map "\C-c\C-c" 'rmail-cease-edit)
  (define-key rmail-edit-map "\C-c\C-]" 'rmail-abort-edit)
  (rmail-cease-edit)
  (pop-to-buffer rmail-summary-buffer))

(defun rmail-summary-abort-edit ()
  "Abort edit of current message. Restore original contents."
  (interactive)
  (define-key rmail-edit-map "\C-c\C-c" 'rmail-cease-edit)
  (define-key rmail-edit-map "\C-c\C-]" 'rmail-abort-edit)
  (rmail-abort-edit)
  (pop-to-buffer rmail-summary-buffer))

;;;; *** Rmail Summary Mailing Commands ***

(defun rmail-summary-mail ()
  "Send mail in another window.
While composing the message, use \\[mail-yank-original] to yank the
original message into it."
  (interactive)
  (mail-other-window nil nil nil nil nil rmail-buffer))

(defun rmail-summary-continue ()
  "Continue composing outgoing message previously being composed."
  (interactive)
  (mail-other-window t))

(defun rmail-summary-reply (just-sender)
  "Reply to the current message.
Normally include CC: to all other recipients of original message;
prefix argument means ignore them.
While composing the reply, use \\[mail-yank-original] to yank the
original message into it."
  (interactive "P")
  (pop-to-buffer rmail-buffer)
  (rmail-reply just-sender)
  (define-key mail-mode-map "\C-c\C-c" 'rmail-summary-reply-send-and-exit))

(defun rmail-summary-reply-send-and-exit ()
  "Send mail reply and return to summary buffer."
  (interactive)
  (define-key mail-mode-map "\C-c\C-c" 'mail-send-and-exit)
  (mail-send-and-exit t))

(defun rmail-summary-forward ()
  "Forward the current message to another user."
  (interactive)
  (set-buffer rmail-buffer)
  (rmail-forward))

(defun rmail-summary-expunge-and-save ()
  "Expunge and save RMAIL file."
  (interactive)
  (set-buffer rmail-buffer)
  (rmail-expunge-and-save)
  (pop-to-buffer rmail-summary-buffer))

;;;; *** Rmail Edit Mode ***

(autoload 'rmail-summary-edit-current-message "rmailedit"
  "Edit the contents of the current message"
  t)


-- 
Bob Weiner, Motorola, Inc.,   USENET:  ...!gatech!uflorida!novavax!weiner
(407) 738-2087


