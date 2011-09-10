;To: unix-emacs@BBN.COM
;Date: 19 May 89 16:25:10 GMT
;From: "Mark D. Baushke" <apple!oliveb!3comvax!bridge2!mdb@BBN.COM>
;Sender: arpa-unix-emacs-request@BBN.COM
;Subject: full-featured version of time.el (was Re: display-time)
;References: <24576@agate.BERKELEY.EDU>, <40157@bbn.COM>
;Organization: 3Com Corp., Mountain View, CA.
;Source-Info:  From (or Sender) name not authenticated.
;
;In article <40157@bbn.COM> jr@bbn.com (John Robinson) writes:
;>In article <24576@agate.BERKELEY.EDU>, alanw@django (Alan Weinstein) writes:
;>>Is it possible to eliminate the "load" information from the status
;>>line when display-time is invoked?  
;>
;>display-time simply runs the program `loadst'.  Normally, this
;>resolves to /usr/local/emacs/etc/loadst, but if your exec-path
;>variable has a directory earlier than that one that contains an
;>executable loadst, it will be used instead.  So you can modify
;>loadst.c to make a private version, or better yet, simply make it the
;>shellscript:
;>
;>while true
;>do date +%r
;>   sleep $2
;>done
;
;Alternatively, you can modify time.el to do what you want. I did this
;starting back in 1987 and have kept it up with each version since.
;
;The following is a 'full featured' (maybe too many?) version of
;time.el which I am currently running under GNU Emacs 18.51. It should
;work with 18.54 with no problems since time.el has not changed between
;18.51 and 18.54.
;
;Features of interest:
;
;	* Provides a hook to 'beep' on new mail arrival.
;
;	* load information can be suppressed completely. A zero load
;	  0.00[0] is always supressed
;
;	* time can be suppressed (only the Mail flag will be displayed)
;
;	* A simple timer mechanism can tell you to 'go home' at a
;	  particular time each day or may be used as a reminder
;	  service by setting display-gohome-time like
;
;	(setq display-gohome-time '(
;				    ("Wed" 1115 "Dentist Appt." 2030)
;				    ("Thu" 1845 "Pick up kids" 2030)
;				    ))
;
;I find it quite handy. Just remember to load it from you ~/.emacs
;before your (display-time).
;
;N.B.: This code should probably use the save-match-data stuff recently
;posted, but I don't have time to add that right now. Feel free to do so.
;
;Improvements welcome.
;
;Enjoy!
;--
;Mark D. Baushke
;UUCP:	    {3comvax,auspex,sun}!bridge2!mdb
;Nameserver: mdb@bridge2.ESD.3Com.COM

;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; my-time.el --- modified 18.51 time.el
;; 
;; Author          : Mark D. Baushke
;; Created On      : Sep 15, 1987
;; Last Modified By: Mark D. Baushke
;; Last Modified On: Thu Mar  9 09:38:28 1989
;; Update Count    : 2
;; Status          : OK.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Display time and load in mode line of Emacs.
;; Copyright (C) 1985, 1986, 1987 Free Software Foundation, Inc.

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


;; A version of time that gets rid of the insignificant parts of the
;; display-time line "[0]", as well as the 0.00 if the load is that
;; low. Optionally, setting the variables 'display-newmail-beep will
;; print a message when it first notices that mail has arrived.  Using
;; display-mail-only will get rid of all load and time information,
;; just prints the "Mail" flag when you have some.

(defvar display-time-process nil)


(defvar display-time-no-load nil
  "*If non-NIL, do not display any load information.")

(defvar display-mail-only nil
  "*Only print the Mail flag not the time or load.")

(defvar display-newmail-beep nil
  "*Print a message when new mail first arrives.")

(defvar display-gohome-msg "Go Home"
  "*Add this message to the display-time-string when display-gohome-time.")

(defvar display-gohome-time nil
  "*Add the display-gohome-msg to the display-time-string.
The value of the variable may be a number or string representing military time
(e.g., 1800 is 6pm, if it is a string it may optionally have a ':'
between the hours and the minutes). Or it may be an association list
between the 'day of the week', 'military time', and optional
gohome-msg of the form:
(setq display-gohome-time '((\"Mon\" 1900) (\"Tue\" 1830 \"Tue meeting\"))) 
Which would print the Go Home message only on Monday at 7pm and
Tuesday at 6:30pm.")

(defvar display-time-interval 60
  "*Seconds between updates of time in the mode line.")

(defvar display-time-string nil)

(defun display-time ()
  "Display current time and load level in mode line of each buffer.
Updates automatically every minute.
If display-time-day-and-date is non-nil, the current day and date
are displayed as well."
  (interactive)
  (let ((live (and display-time-process
		   (eq (process-status display-time-process) 'run))))
    (if (not live)
	(save-excursion
	  (if display-time-process
	      (delete-process display-time-process))
	  (or global-mode-string (setq global-mode-string '("")))
	  (or (memq 'display-time-string global-mode-string)
	      (setq global-mode-string
		    (append global-mode-string '(display-time-string))))
	  (setq display-time-string "time and load")
	  (setq display-time-process
		(start-process "display-time" nil
			       "loadst" 
			       "-n" (int-to-string display-time-interval)))
	  (process-kill-without-query display-time-process)
	  (set-process-sentinel display-time-process 'display-time-sentinel)
	  (set-process-filter display-time-process 'display-time-filter)))))

(defun display-time-sentinel (proc reason)
  (or (eq (process-status proc) 'run)
      (setq display-time-string ""))
  ;; Force mode-line updates
  (save-excursion (set-buffer (other-buffer)))
  (set-buffer-modified-p (buffer-modified-p))
  (sit-for 0))

(defun military-time (mil-string)
  (if (not (stringp mil-string))
      mil-string
    (if (string-match ":" mil-string)
	(string-to-int
	 (concat (substring mil-string 0 (match-beginning 0))
		 (substring mil-string (1+ (match-beginning 0)))))
      (string-to-int mil-string))))


(defun display-time-filter (proc string)
  ;; Desired data can't need more than the last 30 chars,
  ;; so save time by flushing the rest.
  ;; This way, if we have many different times all collected at once,
  ;; we can discard all but the last few very fast.
  (setq save-filter-string string)
  (if (> (length string) 30)
      (setq string (substring string -30)))
  ;; Now discard all but the very last one.
  (while (and (> (length string) 4)
	      (string-match "[0-9]+:[0-9][0-9].." string 4))
    (setq string (substring string (match-beginning 0))))
  (if (string-match "[^0-9][0-9]+:" string)
      (setq string (substring string 0 (1+ (match-beginning 0)))))
  ;; Remove the load part of the string?
  (if display-time-no-load
      (if (string-match
	   "\\( [0-9]+\\.[0-9]+\\)+\\( Mail\\)+\\(\\[[0-9]*\\]\\)" string)
	  (setq string (concat
			(substring string 0 (match-beginning 1))
			(substring string (match-end 1) (match-beginning 3))))
	(if (string-match " [0-9]+\\.[0-9]+\\[[0-9]*\\]" string)
	    (setq string (concat (substring string 0 (match-beginning 0))
				 (substring string (match-end 0))))))
    ;; Even if the user wants the load displayed, do not display trivial load.
    ;; Remove the "0.00[0]" as being insignificant
    (if (string-match " 0.00 Mail[[]0]" string)
	(setq string (concat (substring string 0 (1+ (match-beginning 0)))
			     "Mail")))
    (if (string-match " 0.00[[]0]" string)
	(setq string (substring string 0 (1+ (match-beginning 0)))))
    ;; Remove the "[0]" as being insignificant
    (if (string-match "[[]0]" string)
	(setq string (substring string 0 (match-beginning 0)))))

  ;; Append the date if desired.
  (if display-time-day-and-date
      (setq string (concat (substring (current-time-string) 4 11) string)))

  (if display-mail-only
      (setq string (if (string-match "Mail" string)
		       "Mail"
		     "")))

  (if (and display-newmail-beep
	   (not (string-match "Mail" display-time-string))
	   (string-match "Mail" string))
      (progn
	(message "You have mail waiting...")
	(beep t)
	(sit-for 0)))

  ;; Add the gohome message if necessary.
  (if display-gohome-time
      (progn
	(let ((gohome-time (military-time display-gohome-time))
	      (right-now (string-to-int
			  (concat (substring (current-time-string) 11 13)
				  (substring (current-time-string) 14 16))))
	      (msg display-gohome-msg)
	      stop-time)
	  (if (listp display-gohome-time)
	      (progn
		(setq gohome-time
		      (nth 1 (assoc (substring (current-time-string) 0 3)
				    display-gohome-time)))
		(setq msg
		      (nth 2 (assoc (substring (current-time-string) 0 3)
				    display-gohome-time)))
		(setq stop-time
		      (nth 3 (assoc (substring (current-time-string) 0 3)
				    display-gohome-time)))
		(if (not (stringp msg))
		    (setq msg display-gohome-msg))
		(if (stringp gohome-time)
		    (setq gohome-time (military-time gohome-time)))
		(if (not stop-time)
		    (setq stop-time 2400))
		))
	  (if (numberp gohome-time)
	      (if (and (< gohome-time right-now)
		       (>= stop-time   right-now))
		  (setq string (concat msg " " string))
		(if (= gohome-time right-now)
		    (progn
		      (setq string (concat "*** " msg " *** " string))
		      (message "Time to leave!")
		      (beep t)
		      ))
	      )
	    )
	  )))
  ;; Install the new time for display.
  (setq display-time-string string)
  ;; Force redisplay of all buffers' mode lines to be considered.
  (save-excursion (set-buffer (other-buffer)))
  (set-buffer-modified-p (buffer-modified-p))
  ;; Do redisplay right now, if no input pending.
  (sit-for 0))

