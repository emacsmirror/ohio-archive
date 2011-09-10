;From arpa-unix-emacs-request@ALEXANDER.BBN.COM Fri Oct 16 12:49:27 1987
;Received: from alexander by ALEXANDER.BBN.COM id aa04557; 16 Oct 87 7:57 EDT
;Received: from [128.89.0.122] by ALEXANDER.BBN.COM id aa04553;
;          16 Oct 87 7:57 EDT
;Received: from ucbvax.berkeley.edu by BBN.COM id aa04917; 16 Oct 87 7:56 EDT
;Received: by ucbvax.Berkeley.EDU (5.58/1.27)
;	id AA09645; Fri, 16 Oct 87 04:43:34 PDT
;Received: from USENET by ucbvax.Berkeley.EDU with netnews
;	for arpa-unix-emacs@bbn.com (unix-emacs@bbn.com)
;	(contact usenet@ucbvax.Berkeley.EDU if you have questions)
;Date: 16 Oct 87 06:20:56 GMT
;From: Wolfgang Rupprecht <mgm.mit.edu!wolfgang%bloom-beacon.mit.edu.uucp@BBN.COM>
;Organization: Independent Software Consultant
;Subject: popmail, a pop up mail buffer
;Message-Id: <1623@bloom-beacon.MIT.EDU>
;Sender: unix-emacs-request@BBN.COM
;To: unix-emacs@BBN.COM
;Status: RO
;
;Here is a handy little mail watcher. This 'widget' checks your
;mailboxs for mail. If it finds any, it runs rmail, and pops up a
;window displaying it.
;
;This code hangs on the time-and-date process, which gets run once a
;minute.
;
;The effect is somewhat startling the first time a window pops out of
;nowhere, but you soon become hooked on having your mail displayed the 
;moment it arrives.
;
;---- replace display-time-filter in time.el with this -----
;; Copyright (C) 1985, 1986, 1987 Free Software Foundation, Inc.
;; Read the GNU COPYING file for the full details.
;; 9/17/87 wolfgang@mgm.mit.edu

(defvar pop-mail nil "*If t display new mail in a pop up mail buffer.
	files listed in rmail-primary-inbox-list are watched")

(defun display-time-filter (proc string)
  "A filter that replaces the 'display-time-filter' filter.
This filter pops up a mail window whenever new mail arrives."
  ;; Desired data can't need more than the last 30 chars,
  ;; so save time by flushing the rest.
  ;; This way, if we have many different times all collected at once,
  ;; we can discard all but the last few very fast.
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
  ;; Install the new time for display.
  (setq display-time-string string)
  ;; Force redisplay of all buffers' mode lines to be considered.
  (save-excursion (set-buffer (other-buffer)))
  (set-buffer-modified-p (buffer-modified-p))
  ;; Do redisplay right now, if no input pending.
  (sit-for 0)
  (if pop-mail
      (let ((list rmail-primary-inbox-list)
	    file)
	(while list
	  (if (and (file-exists-p
		     (setq file (expand-file-name
				  (substitute-in-file-name (car list)))))
		   (/= 0 (nth 7 (file-attributes file)))) ;size != 0
	      (progn
		(save-excursion (rmail))
		(display-buffer (get-buffer "RMAIL"))
		(setq list nil))	; once only
	    (setq list (cdr list)))))))


;Wolfgang Rupprecht	  UUCP: mirror!mit-mgm!wolfgang
;			  ARPA: wolfgang@mgm.mit.edu (IP addr 18.82.0.114)

