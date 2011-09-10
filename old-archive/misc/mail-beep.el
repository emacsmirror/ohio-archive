;From ark1!nems!mimsy!haven!purdue!tut.cis.ohio-state.edu!cs.utexas.edu!samsung!think!leander.think.com!fad Wed Dec  6 12:17:44 1989
;Article 766 of gnu.emacs:
;Path: ark1!nems!mimsy!haven!purdue!tut.cis.ohio-state.edu!cs.utexas.edu!samsung!think!leander.think.com!fad
;>From fad@leander.think.com (Franklin A Davis)
;Newsgroups: gnu.emacs
;Subject: Re: Notify on mail arrival (biff) in mode-line
;Keywords: emacs, mail, mode line
;Message-ID: <31755@news.Think.COM>
;Date: 21 Nov 89 22:21:00 GMT
;References: <1989Nov20.182913.23723@athena.mit.edu>
;Sender: news@Think.COM
;Reply-To: fad@leander.think.com.UUCP (Franklin A Davis)
;Organization: Thinking Machines Corporation
;Lines: 95
;
;In article <1989Nov20.182913.23723@athena.mit.edu> boaz@lcs.mit.edu writes:
;>
;>
;>Has someone hacked up a way to signal in the mode line 
;>that new mail has arrived ?? I guess it take a process
;>to run and check periodically the size of
;>/usr/spool/mail/$user .
;>Can the display-time process be modified to do that ??
;>
;>-- Boaz Ben-Zvi  (boaz@lcs.mit.edu)
;
;
;Yes.  Here's a hack from Mark Ardis, now of SEI, CMU, Pittsburgh.  It
;displays the word "Mail" in the mode line whenever you have mail
;waiting.  It beeps when mail first arrives.
;
;Note that the emacs/etc/loadst program must have the correct
;ownership, group, and setuid for it to work:
;
;-rwxr-sr-x  1 root     kmem        24576 Mar  6  1989 loadst*
;
;This is accomplished by root executing the following three commands
;in the emacs/etc directory (wherever that is on your machine):
;"chgrp kmem loadst" 
;"chown root loadst"
;"chmod 2755 loadst"
;
;Here's mail-beep.el.  Remember to run "M-X byte-compile" on it.
;Put the file wherever you want and load it in your .emacs file:
;
;(load "wherever/mail-beep")
;
;Then, to turn it on, include the following in your .emacs file:
;
;(display-time)
;
;Enjoy!
;
;--Franklin


;;; cut here
;;; $Header: /tmp_mnt/am/p7/utility/gmacs/f3/RCS/mail-beep.el,v 1.1 88/12/27 17:06:42 fad Exp $

;; 10/20/87 Franklin Davis, Thinking Machines Corp. (fad@think.com) 
;; Modified display-time-filter to beep and display message when new mail 
;; arrives.  Idea and implementation from Mark Ardis (maa@sei.cmu.edu)

(defvar display-time-process nil)
(defvar seen-mail nil)  

(defun display-time-filter (proc string)
  ;; Desired data can't need more than the last 30 chars,
  ;; so save time by flushing the rest.
  ;; This way, if we have many different times all collected at once,
  ;; we can discard all but the last few very fast.
  (let (idx mail-idx mail-notice)
    (setq mail-idx (string-match "\\[" string))
    (setq mail-notice (substring string (- mail-idx 4) mail-idx))
    (while (setq idx (string-match "]." string))
      (setq string (substring string (1+ idx))))
    (if (> (length string) 30)
	(setq string (substring string -30)))
    ;; Now discard all but the very last one.
    (while (and (> (length string) 4)
		(string-match "[0-9]+:[0-9][0-9].." string 4))
      (setq string (substring string (match-beginning 0))))
    (if (string-match "[^0-9][0-9]+:" string)
	(setq string (substring string 0 (1+ (match-beginning 0)))))
    ;; Append the date if desired.
    (if display-time-day-and-date
	(setq string (concat (substring (current-time-string) 0 11) string)))
    (if (string-equal mail-notice "Mail")
	(if (not seen-mail)
	    (progn
	      (ding)
	      (ding)
	      (message "You have new mail.")
	      (setq seen-mail t)
	      )				; progn
	  )				; if
      (setq seen-mail nil)
      )					; if

    ;; Install the new time for display.
    (setq display-time-string string)
    ;; Force redisplay of all buffers' mode lines to be considered.
    (save-excursion (set-buffer (other-buffer)))
    (set-buffer-modified-p (buffer-modified-p))
    ;; Do redisplay right now, if no input pending.
    (sit-for 0)))

;  franklin a davis  Thinking Machines Corp. Cambridge, MA 02142   617-876-1111
;  <fad@think.com>   {ames, harvard, mit-eddie, uunet}!think!fad 
;				Let the four winds blow you safely home!


