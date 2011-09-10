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

;; The following code implements a Pcmail client under GNU-EMACS.  For
;; details on how Pcmail works, see NIC RFC-993.

;;;; global variable definitions

;;;; pcmail mail composition commands -- compose, forward, reply

(defun pcmail-mail ()
  "Compose mail in another window.
Args: none
  Compose mail in another window.  Any header parsing can be made
to conform to NIC RFC-822 by setting the variable mail-use-rfc822 non-NIL."
  (interactive)

  ; note the skullduggery here.  We need to set the referencing message
  ; and folder now because once we call mail-other-window we lose the
  ; pcmail local variables.  We can't set the referencing variables
  ; now because they are local variables defined in 
  ; pcmail-overlay-mail-commands
  (let ((n pcmail-current-subset-message)
	(folder pcmail-folder-name))
    (pcmail-mail-other-window nil nil nil nil nil (current-buffer))
    (pcmail-overlay-mail-commands n folder)))

(defun pcmail-mail-subset ()
  "Compose mail in another window, with mail text being the current subset.
Args: none
  Like pcmail-mail, but inserts the messages comprising the current subset
into the mail buffer.  Those messages with Summary-Line: fields will have
them placed at the head of the message in a table of contents."
  (interactive)

  ; note the skullduggery here.  We need to set the referencing message
  ; and folder now because once we call mail-other-window we lose the
  ; pcmail local variables.  We can't set the referencing variables
  ; now because they are local variables defined in 
  ; pcmail-overlay-mail-commands
  (let ((n pcmail-current-subset-message)
	(folder pcmail-folder-name)
	(mailbuf)
	(folderbuf (current-buffer))
	(absnum)
	(start)
	(tocstart)
	(subset-len)
	(end)
	(this-toc)
	(toc-text)
	(i 1))
    (pcmail-mail-other-window nil nil nil nil nil (current-buffer))
    (pcmail-overlay-mail-commands n folder)
    (setq mailbuf (current-buffer)
	  tocstart (point-max))
    (save-excursion
      (save-restriction
	(set-buffer folderbuf)
	(setq toc-text 
	      (concat "\n***** Contents: message set of " 
		      (int-to-string (pcmail-current-subset-length))
		      " messages *****\n"))
	(setq subset-len (pcmail-current-subset-length))
	(while (<= i subset-len)
	  (setq absnum (pcmail-make-absolute i))
	  (pcmail-narrow-to-unpruned-header absnum)
	  (setq this-toc
		(format "%4d %s" i
			(or (mail-fetch-field "summary-line")
			    "[no summary available]")))
	  (setq toc-text 
		(concat toc-text "\n" 
			(substring this-toc 0 (min (1- (window-width))
						   (length this-toc)))))
	  (pcmail-narrow-to-message absnum)
	  (setq start (point-min)
		end (point-max))
	  (save-excursion
	    (set-buffer mailbuf)
	    (goto-char (point-max))
	    (insert (format "\n\n***** Message %d/%d *****\n\n" i subset-len))
	    (insert-buffer-substring folderbuf start end))
	  (setq i (1+ i)))))
    (save-excursion
      (goto-char tocstart)
      (insert toc-text))))

(defun pcmail-answer-message (just-sender)
  "Answer the current message.
Args: (just-sender)
  Compose a reply to the current message.  Set this message's answered
attribute.  If JUST-SENDER is non-nil (interactively with prefix argument),
Do not include CC: to all other recipients of original message, otherwise
do.  While composing the reply, use \\[mail-yank-original] to yank the
original message into it.

Any header parsing can be made to conform to NIC RFC-822 by setting
the variable mail-use-rfc822 non-NIL.

If the variable pcmail-yank-original is non-NIL, yank the original
message into the mail buffer."
  (interactive "P")
  (pcmail-barf-if-empty-folder)
  (let ((from) 
	(reply-to)
	(message-id)
	(cc)
	(subject)
	(date)
	(to)
	(resent-p)
	(n (pcmail-make-absolute pcmail-current-subset-message))
	(relnum pcmail-current-subset-message)
	(folder pcmail-folder-name))
    (save-excursion
      (save-restriction
	(pcmail-narrow-to-unpruned-header n)
	(setq to (or (setq resent-p (mail-fetch-field "resent-to" nil t))
		     (setq resent-p (mail-fetch-field "resent-apparently-to" 
						      nil t)) ;uck
		     (mail-fetch-field "to" nil t)
		     (mail-fetch-field "apparently-to" nil t) ;uck
		     ""))
	(cond (resent-p
		(setq from (mail-fetch-field "resent-from")
		      reply-to (or (mail-fetch-field "resent-reply-to" t)
				   from)
		      cc (if just-sender nil (mail-fetch-field "resent-cc" t))
		      subject (or (mail-fetch-field "resent-subject" t)
				  (mail-fetch-field "subject" t))
		      date (mail-fetch-field "resent-date" t)
		      message-id (or (mail-fetch-field "resent-message-id" t)
				     "")))
	      (t
		(setq from (mail-fetch-field "from")
		      reply-to (or (mail-fetch-field "reply-to" t)
				   from)
		      cc (if just-sender nil (mail-fetch-field "cc" nil t))
		      subject (mail-fetch-field "subject" t)
		      date (mail-fetch-field "date")
		      message-id (or (mail-fetch-field "message-id")
				     ""))))))

    ; if subject was a Re:, strip the Re: since one will be added later
    (and subject
	 (string-match "^[ \t]*\\(re:[ \t]*\\)*\\(.*\\)" subject)
	 (setq subject (substring subject (match-beginning 2) (match-end 2))))

    ; and compose the message
    (pcmail-mail-other-window nil
      (mail-strip-quoted-names reply-to)
      (and subject (concat "Re: " subject))
      (pcmail-make-in-reply-to-field from date message-id)
      (cond (just-sender
	      nil)
	    (t
	      (let* ((cc-list (rmail-dont-reply-to
				(mail-strip-quoted-names
				  (if (null cc) to (concat to ", " cc))))))
		(if (string= cc-list "") nil cc-list))))
      (current-buffer)
      (list (list '(lambda (folder n rel)
		     (save-excursion
		       (save-restriction
			 (pcmail-open-folder folder)
			 (pcmail-narrow-to-message n)
			 (pcmail-set-attribute n "answered" t)
			 (pcmail-update-folder-mode-line rel))))
		  pcmail-folder-name 
		  (pcmail-make-absolute pcmail-current-subset-message)
		  pcmail-current-subset-message)))
    (pcmail-overlay-mail-commands relnum folder)
    (pcmail-maybe-yank-original)))

(defun pcmail-maybe-yank-original ()
  "Maybe yank replied-to message into reply body.
Args: none
  If pcmail-yank-message-on-reply is non-NIL, place the replied-to message
in the message reply.  The header is filtered through the
mail-yank-ignored-headers regexp and is indented.  If 
pcmail-yank-message-on-reply is a string, highlight the yanked message
with that string at the beginning of each line."
  (and pcmail-yank-message-on-reply
       (pcmail-insert-current-message nil)))

(defun pcmail-make-in-reply-to-field (from date message-id)
  " Create an in-reply-to field from from:, date:, and message-id: fields.
Args: (from date message-id)"
  (let ((field))
    (and from (setq field (pcmail-quoted-name from)))
    (and date (setq field (concat field "'s message of " date)))
    (and message-id (setq field (concat field " " message-id)))))

(defun pcmail-quoted-name (field)
  "Return FIELD's quoted name or FIELD if no quoted name exists.
Args: (field)."
  (if (or (string-match "\\(.*\\)  *<" field)
	  (string-match "(\\(.*\\))" field))
      (setq field (substring field (match-beginning 1) (match-end 1)))
    field))

(defun pcmail-forward-message (dont-clear)
  "Forward the current message.
Args: dont-clear
  Set up a forwarded message for editing by the user.  Forwarded messages
are given the forwarded attribute.  If called interactively, a prefix
arg means do not filter the header of the forwarded message prior to insertion
in the mail composition buffer.  If pcmail-highlight-forwarded-message
is non-NIL, put highlight lines at the beginning and end of the forwarded
text.   If the variable is a string, use it as the highlight text.  Any 
header parsing can be made to conform to NIC RFC-822 by setting the variable
mail-use-rfc822 non-NIL."
  (interactive "P")
  (pcmail-barf-if-empty-folder)
  (let ((forward-buffer (current-buffer))
	(start) (subject (mail-fetch-field "subject"))
	(forwarded-from)
	(n pcmail-current-subset-message)
	(folder pcmail-folder-name))

    ; if subject was of a forwarded message, strip the subject portion to
    ; avoid cascaded [foo: [foo: [foo: ...]]] nightmares
    (and subject
	 (let ((match) (lastgood))
	   (while (setq match (string-match "\\[.*: " subject lastgood))
	     (setq lastgood (match-end 0)))
	   (and lastgood
		(setq subject (substring subject lastgood))
		(and (setq match (string-match "\\]+" subject))
		     (setq subject (substring subject 0 match))))))
    (setq forwarded-from (mail-strip-quoted-names (mail-fetch-field "From")))
    (pcmail-mail-other-window nil nil 
	  (if subject
	      (format "[%s: %s]" forwarded-from subject)
	    (format "[message from %s]" forwarded-from))
	  nil nil 
	  (current-buffer)
	  (list (list '(lambda (folder n rel)
			 (save-excursion
			   (save-restriction
			     (pcmail-open-folder folder)
			     (pcmail-narrow-to-message n)
			     (pcmail-set-attribute n "forwarded" t)
			     (pcmail-update-folder-mode-line rel))))
		      pcmail-folder-name 
		      (pcmail-make-absolute pcmail-current-subset-message)
		      pcmail-current-subset-message)))
    (save-excursion
      (goto-char (point-max))
      (forward-line 1)
      (and pcmail-highlight-forwarded-message
	   (if (stringp pcmail-highlight-forwarded-message)
	       (insert "\n" pcmail-highlight-forwarded-message "\n\n")
	     (insert "\n---Begin Forwarded Message---\n\n")))
      (setq start (point))
      (insert-buffer forward-buffer)
      (or dont-clear (mail-yank-clear-headers start (mark)))
      (goto-char (point-max))
      (and pcmail-highlight-forwarded-message
	   (if (stringp pcmail-highlight-forwarded-message)
	       (insert "\n" pcmail-highlight-forwarded-message "\n\n")
	     (insert "\n\n---End Forwarded Message---\n"))))
    (pcmail-overlay-mail-commands n folder)))

(defun pcmail-overlay-mail-commands (relnum folder)
  "Overlay standard mail functions with augmented pcmail functions.
Args: (relnum folder)
  Also set local variables which will tell the mail-send routine whether
or not to set referencing message attributes on send."
  (make-local-variable 'pcmail-referencing-message-rel-number)
  (setq pcmail-referencing-message-rel-number relnum)
  (make-local-variable 'pcmail-referencing-message-folder)
  (setq pcmail-referencing-message-folder folder)
  (define-key mail-mode-map "\C-c\C-y" 'pcmail-insert-current-message)
  (define-key mail-mode-map "\C-c\C-c" 'pcmail-mail-send-and-exit))

(defun pcmail-mail-send-and-exit (arg)
  "Like mail-send-and-exit, but sensitive to the other buffer being 
pcmail-mode rather than rmail-mode."
  (interactive "P")
  (mail-send)
  (bury-buffer (current-buffer))
  (if (and (not arg)
	   (not (one-window-p))
	   (save-excursion
	     (set-buffer (window-buffer (next-window (selected-window) 'not)))
	     (eq major-mode 'pcmail-folder-mode)))
      (delete-window)
    (switch-to-buffer (other-buffer (current-buffer)))))

(defun pcmail-insert-current-message (n)
  "Insert the current folder's current message into an outbound message.
Args: (n)
  Insert the current message into the message composition buffer and filter
its header according to mail-yank-ignored-headers.  With a
prefix argument N, insert the Nth message in the current subset.  If N
is negative, don't filter the inserted message's header."
  (interactive "P")
  (let ((nostrip) (folderbuf) (msgbuf (current-buffer)))
    (if (or (null n)
	    (zerop n))
	(setq n pcmail-referencing-message-rel-number))
    (and n
	 (save-excursion
	   (if (< n 0) 
	       (setq nostrip t
		     n (- n)))
	   (cond ((pcmail-find-folder pcmail-referencing-message-folder)
		  (pcmail-open-folder pcmail-referencing-message-folder)
		  (save-excursion
		    (save-restriction
		      (setq n (pcmail-make-absolute n))
		      (cond ((<= n pcmail-total-messages)
			     (pcmail-narrow-to-message n)
			     (setq folderbuf (current-buffer))
			     (delete-windows-on folderbuf)
			     (set-buffer msgbuf)
			     (insert-buffer folderbuf)
			     (or nostrip 
				 (run-hooks
				  (if (and (boundp 'mail-yank-hooks)
					   mail-yank-hooks)
				      'mail-yank-hooks
				    'pcmail-default-mail-yank-hook)))
			     (exchange-point-and-mark)
			     (or (eolp)
				 (insert ?\n))))))))))))

(defun pcmail-mail-other-window (&rest args)
  "Send mail in a new frame if pcmail-new-frame-on-mail is non-NIL
Args: (&rest args)
  If pcmail-new-frame-on-mail is non-NIL, compose mail in another frame,
otherwise compose it in another window."
  (cond (pcmail-new-frame-on-mail
	 (apply 'mail-other-frame args)
	 (modify-frame-parameters (selected-frame) '((dedicated . t))))
	(t
	 (apply 'mail-other-window args))))

;;; yank-message hook, for use with supercite package.  Default hook assumes
;;; no supercite capability

(setq pcmail-default-mail-yank-hook
      '(lambda ()
	 (let ((start (point)))
	   (mail-yank-clear-headers start (mark))
	   (indent-rigidly start (mark) 3)
	   (and (stringp pcmail-yank-prefix)
		(while (re-search-forward "^" nil t)
		  (replace-match pcmail-yank-prefix)
		  (forward-line 1))))))

(provide 'pcmailmail)
