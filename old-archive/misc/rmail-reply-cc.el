;;  rmail-reply-cc.el
;;  automatically CC's the author of replies to GNU Emacs rmail messages.
;;
;;  written by Lee Short (short@asf.com)
;;  Copyright (C) 1992 Lee Short.
;;  last mod: 20 November, 1992

;; LCD Archive Entry:
;; rmail-reply-cc|Lee Short|short@asf.com|
;; Automatically CC the author of replies to GNU Emacs rmail messages.|
;; 92-11-20||~/misc/rmail-reply-cc.el.Z|

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation.  

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a copy of the GNU General Public License write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


(defvar  my-mail-address "foo@bar.com" 
"The users email address as it will be automaticlly inserted on CC: 
lines.")

(defvar  auto-cc nil 
"The value of this variable determines if the user will be automatically 
CCed on mail messages.  If maillist.el is also used, then this
variable is overridden by the three auto-cc-on-* variables found in 
that file." )

(defun rmail-reply (just-sender)
  "Reply to the current message.
Normally include CC: to all other recipients of original message;
prefix argument means ignore them.
While composing the reply, use \\[mail-yank-original] to yank the
original message into it."
  (interactive "P")
  ;;>> this gets set even if we abort. Can't do anything about it, though.
  (rmail-set-attribute "answered" t)
  (rmail-display-labels)
  (let (from reply-to cc subject date to message-id resent-reply-to)
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (rmail-msgbeg rmail-current-message))
	(forward-line 1)
	(if (= (following-char) ?0)
	    (narrow-to-region
	     (progn (forward-line 2)
		    (point))
	     (progn (search-forward "\n\n" (rmail-msgend rmail-current-message)
				    'move)
		    (point)))
	  (narrow-to-region (point)
			    (progn (search-forward "\n*** EOOH ***\n")
				   (beginning-of-line) (point))))
	(setq resent-reply-to (mail-fetch-field "resent-reply-to" t)
	      from (mail-fetch-field "from")
	      reply-to (or resent-reply-to
			   (mail-fetch-field "reply-to" nil t)
			   from)
	      cc (cond (just-sender nil)
		       (resent-reply-to (mail-fetch-field "resent-cc" t))
		       (t (mail-fetch-field "cc" nil t)))
	      subject (or (and resent-reply-to
			       (mail-fetch-field "resent-subject" t))
			  (mail-fetch-field "subject"))
	      date (cond (resent-reply-to
			  (mail-fetch-field "resent-date" t))
			 ((mail-fetch-field "date")))
	      to (cond (resent-reply-to
			(mail-fetch-field "resent-to" t))
		       ((mail-fetch-field "to" nil t))
		       ;((mail-fetch-field "apparently-to")) ack gag barf
		       (t ""))
	      message-id (cond (resent-reply-to
				(mail-fetch-field "resent-message-id" t))
			       ((mail-fetch-field "message-id"))))))
    (and subject
	 (string-match "\\`Re: " subject)
	 (setq subject (substring subject 4)))
    (mail-other-window nil
      (mail-strip-quoted-names reply-to)
      subject
      (rmail-make-in-reply-to-field from date message-id)
      (let  ((cc-string
	      (if just-sender
		  nil
  	          (let* ((cc-list (rmail-dont-reply-to
		  	            (mail-strip-quoted-names
			              (if (null cc) to (concat to ", " cc))))))
		     (if (string= cc-list "") nil cc-list)))))
         (if  (null cc-string)
              (if auto-cc my-mail-address nil)
              (if auto-cc
                  (concat cc-string ", " my-mail-address)
                  cc-string)))
      (current-buffer))))
