;To: unix-emacs@bbn.com
;Date: 3 Feb 89 16:19:10 GMT
;From: Dan Pierson <encore!mist@bu-cs.bu.edu>
;Subject: Re: RMAIL file ---> UNIX mail file
;
;In article <156@ai.cs.utexas.edu>, yvo@cs (Yvonne van Olphen) writes:
;>
;>I accidentally started up RMAIL while in emacs, and it sucked up all
;>my (30+) messages that were in my spool file.  I don't know RMAIL, and
;>I want my messages back in a UNIX mail file without having to C-o
;>*every single* message individually.  Is there some way I can
;>completely convert the RMAIL file back into a UNIX mail file
;>painlessly??  HELP!
;>
;>	-Yvonne
;Well, I haven't used this in a while, but the following code may help.
;In response to an earlier question: one way to get from RMAIL/BABYL
;format to MH format is to convert back to UNIX mail format, then use
;``inc +mh-mailbox -file unix-mailfile''.

(defun rmail-apply (function &rest args)
  "Apply a function to all messages.
For each message, FUNCTION is applied to the ARGS."
  (message "Applying function to all messages...")
  (let ((msgnum 1)
	(buffer-read-only nil))
    (save-restriction
      (save-excursion
	(widen)
	(goto-char (point-min))
	(while (>= rmail-total-messages msgnum)
	  (rmail-show-message msgnum)
	  (or (null function)
	      (apply function args))
	  (setq msgnum (1+ msgnum))))))
  (message "Applying function to all messages...done"))

(defvar babyl-file-root "~/mail/"
  "Pathname of the directory where BABYL files live")

(defun babyl-to-mail (name)
  (interactive "sMail File: ")
  (let ((in (concat babyl-file-root name))
	(out (concat "~/" name ".mbox")))
    (rmail in)
    (rmail-apply 'rmail-output out)
    (rmail-quit)))
;-- 
;                                            dan
;
;In real life: Dan Pierson, Encore Computer Corporation, Research
;UUCP: {talcott,linus,necis,decvax}!encore!pierson
;Internet: pierson@multimax.encore.com
;
;