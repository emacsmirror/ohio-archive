;To: unix-emacs@bbn.com
;Date: 30 Nov 88 15:40:01 GMT
;From: John Sturdy <mcvax!ukc!harrier.ukc.ac.uk!eagle.ukc.ac.uk!icdoc!qmc-cs!harlqn!jcgs@uunet.uu.net>
;Subject: non-editing modes and code
;
;Well, someone asked for that so-useful editing command to tell you
;when the next train is, so here it is:
;--------------------------------cut here--------------------------------
; -*-emacs-lisp-*- /usr/users/jcgs/emacs/trains.el
; Last edited: Wed Nov 30 15:35:57 1988 by jcgs (John Sturdy) on harlqn

(defun digit-above-regexp (digit)
  "Returns a chunk of regular expression to search for a digit higher
than DIGIT, or if DIGIT is 9, something that will not match any
digit."
  (let ((x (1+ digit)))
    (if (> x 9)
	"[^0-9]"
      (format "[%1d-9]" x))))

(defun time-after (time)
  "Return a regular expression for the earliest time after TIME, which
is given in HHMM format."
  (let ((hour-tens (string-to-int (substring time 0 1)))
	(hour-units (string-to-int (substring time 1 2)))
	(minute-tens (string-to-int (substring time 2 3)))
	(minute-units (string-to-int (substring time 3 4))))
    (format
     "%s%s%s\\|%s[%1d-5][0-9]\\|%s%s[0-5][0-9]\\|[%1d-2][0-9][0-5][0-9]"
     ;; the first possibility is in the next ten minutes of this hour
     (substring time 0 2) (substring time 2 3)
     (digit-above-regexp minute-units)
     ;; now try in the rest of this hour
     (substring time 0 2) (1+ minute-tens)
     ;; otherwise something in the rest of this ten hours
     (substring time 0 1) (digit-above-regexp hour-units)
     ;; otherwise anything later
     (1+ hour-tens))))

(defun train (destination time)
  "Return (and display, if interactive) the next train to DESTINATION
that leaves after TIME. If TIME is nil or the empty string, the
current time is used; otherwise TIME should be a string in the format
HHMM."
  (interactive
   (list (completing-read "To: " '(("Cambridge") ("Foxton")) nil t)
	 (read-from-minibuffer "Time (HHMM) (default: now): ")))
  (let ((now (current-time-string))
	(result-string nil))
  (save-window-excursion
    (if (or (null time) (zerop (length time)))
	(setq time
	      (format "%s%s"
		      (substring now 11 13)
		      (substring now 14 16))))
    (find-file "/harlqn/usr/users/jcgs/misc/TRAINS")
    (goto-char (point-min))
    (search-forward (format "to %s" destination))
    (search-forward (substring now 0 3)) ; day of week
    (re-search-forward (format "\\(%s\\)->[0-9]+" (time-after time)))
    (setq result-string (buffer-substring (match-beginning 0)
					   (match-end 0)))
    (bury-buffer (current-buffer)))
  (if (interactive-p) (message "Next train departs at %s, arrives at %s"
			       (substring result-string 0 4)
			       (substring result-string 6 10)))
  result-string))

; end of trains.el
;--------------------------------cut here--------------------------------
;And here is part of a sample file (really we get more trains than
;this, I just trimmed it to save net costs!):
;--------------------------------cut here--------------------------------
;Foxton to Cambridge
;Mon,Tue,Wed,Thu,Fri
;0021->0034 0043->0100 0108->0125 0638->0652 0759->0811
;2251->2303 2322->2336 0108->0125
;Sat
;0043->0100 0108->0125 0638->0652 0750->0802 0835->0907
;(Sun morning) 0016->0033 0108->0125
;Sun
;0016->0033 0108->0125 0740->0753 0904->0917 1004->1017
;2121->2134 2221->2234 2321->2334 (Mon morning) 0021->0034
;Cambridge to Foxton
;Mon,Tue,Wed,Thu,Fri
;0020->0029 0600->0609 0710->0719 0733->0742 0752->0801
;--------------------------------cut here--------------------------------
;-- 
;__John      All facts are useless, but some facts are more useless than others.
;                            (After Ecclesiastes Chs. 1 & 2, 1 Corinthians 13:9,
;                                             and George Orwell's "Animal Farm")
;         jcgs@uk.co.harlqn Harlequin Ltd,Barrington,Cambridge,UK +44-223-872522
