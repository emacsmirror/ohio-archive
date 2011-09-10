;To: unix-emacs@bbn.com
;Date: 25 Jan 89 23:57:49 GMT
;From: "Randal L. Schwartz @ Stonehenge" <blake!ogccse!littlei!omepd!merlyn@beaver.cs.washington.edu>
;Subject: show-time.el: a time-wasting display hack (src included)
;
;Here's a cute little time-waster I came up with a while ago.  Great
;thing to leave your terminal with (insecurely)... makes it the world's
;most expensive and least portable digital watch.
;
;If you define 'you-have-mail-p', and it returns non-nil when you have
;mail, this screen hack will make use of that knowledge.
;
;See comments for other details.
;
;Flames by email... (if your mailer can get to me, sheesh...)
;-- 
;Randal L. Schwartz, Stonehenge Consulting Services (503)777-0095
;on contract to BiiN (for now :-), Hillsboro, Oregon, USA.
;ARPA: <@iwarp.intel.com:merlyn@intelob.intel.com> (fastest!)
;MX-Internet: <merlyn@intelob.intel.com> UUCP: ...[!uunet]!tektronix!biin!merlyn
;Standard disclaimer: I *am* my employer!
;"Welcome to Oregon... home of the California Raisins!"
;
;--- CUT HERE (no .signature at end, I hope) ---
;;; A little display hack by merlyn
;;; LastEditDate="Fri Nov 25 10:07:40 1988"

;;; Notes:
;;; * format of (current-time-string) must be something like UNIX ctime(3).
;;; * if defined, (you-have-mail-p) returning non-nil will be noticed.
;;; * tcp service "quote" provides the cute remarks -- you might have to
;;;   replace it with a call to "fortune" or "yow" if unavailable

(defun show-time (&optional sleeptime)
  "Display the current time repeatedly in large characters.
Optional SLEEPTIME gives number of seconds between updates (default is
5 seconds).  Any input pending terminates the loop, and restores the
original screen configuration (if the input is a space character, it
is discarded).  If SLEEPTIME is negative, append a cute quote to the
time."
  (interactive "P")
  (setq sleeptime (cond ((null sleeptime) 5)
			((eq sleeptime '-) -10)
			(t (prefix-numeric-value sleeptime))))
  (save-window-excursion
    (switch-to-buffer-other-window (generate-new-buffer " show-time"))
    (unwind-protect
	(progn
	  (delete-other-windows)
	  (set (make-local-variable 'mode-line-inverse-video) inverse-video)
	  (setq mode-line-format "")
	  (while
	      (progn
		(erase-buffer)
		(insert "At the tone, the time will be...\n\n")
		(let* ((ctime (current-time-string))
		       (ignore (string-match "\\(..\\):\\(..\\):\\(..\\)"
					     ctime))
		       (hms (mapcar
			     (function
			      (lambda (n)
				(string-to-int (substring ctime
							  (match-beginning n)
							  (match-end n)))))
			     '(1 2 3))))
		  (show-time-banner (format "    %d:%02d:%02d %s"
					    (1+ (% (+ (nth 0 hms) 11) 12))
					    (nth 1 hms)
					    (nth 2 hms)
					    (if (< (nth 0 hms) 12)
						"AM"
					      "PM"))))
		(message (or (and (fboundp 'you-have-mail-p)
				  (you-have-mail-p)
				  "You have mail.")
			     ""))
		(cond
		 ((< sleeptime 0)
		  (insert "\n\n")
		  (let ((prc (open-network-stream
			      "Quote" nil (system-name) "quote")))
		    (set-marker (process-mark prc) (point-marker))
		    (backward-char 1)
		    (set-process-filter
		     prc
		     (function
		      (lambda (prc txt)
			(let* ((pm (process-mark prc))
			       (buf (marker-buffer pm))
			       (pos (marker-position pm))
			       (md (match-data)))
			  (unwind-protect
			      (if buf
				  (save-excursion
				    (set-buffer buf)
				    (goto-char pos)
				    (insert txt)
				    (save-restriction
				      (narrow-to-region pos (point))
				      (goto-char (point-min))
				      (while (re-search-forward
					      "\^M" nil t)
					(replace-match ""))
				      (goto-char (point-min))
				      (while (re-search-forward
					      "[ -~]\^H" nil t)
					(replace-match "")
					(if (> (point) (point-min))
					    (backward-char 1)))
				      (set-marker pm (point-max)))))
			    (store-match-data md)))))))
		  (sit-for (- sleeptime)))
		 (t (sit-for sleeptime))))))
      (kill-buffer (current-buffer))))
  (and (input-pending-p)
       (= 32 (setq unread-command-char (read-char)))
       (setq unread-command-char -1))
  (sit-for 0))

(defun show-time-banner (string)
  "Insert STRING as tall characteqrs over the next five lines.
Presumes point is already at the beginning of a line.
Used by show-time."
  (insert
   (mapconcat
    (function
     (lambda (line)
       (mapconcat
	(function
	 (lambda (char)
	   (let ((this-char (assq char show-time-banner-chars)))
	     (if this-char (nth line this-char) ""))))
	string
	" ")))
    '(1 2 3 4 5)
    "\n")))
		

(defconst show-time-banner-chars
  '((?0 " 000 "
	"0   0"
	"0   0"
	"0   0"
	" 000 ")
    (?1 "  1  "
	" 11  "
	"  1  "
	"  1  "
	"11111")
    (?2 "2222 "
	"    2"
	" 222 "
	"2    "
	"22222")
    (?3 "3333 "
	"    3"
	" 333 "
	"    3"
	"3333 ")
    (?4 "    4"
	"   44"
	"  4 4"
	" 4444"
	"    4")
    (?5 "55555"
	"5    "
	"5555 "
	"    5"
	"5555 ")
    (?6 " 666 "
	"6    "
	"6666 "
	"6   6"
	" 666 ")
    (?7 "77777"
	"    7"
	"   7 "
	"   7 "
	"   7 ")
    (?8 " 888 "
	"8   8"
	" 888 "
	"8   8"
	" 888 ")
    (?9 " 999 "
	"9   9"
	" 9999"
	"    9"
	" 999 ")
    (?: "::"
	"::"
	"  "
	"::"
	"::")
    (?\040 "  "
	   "  "
	   "  "
	   "  "
	   "  ")
    (?A " AA "
	"A  A"
	"AAAA"
	"A  A"
	"A  A")
    (?M "M   M"
	"MM MM"
	"M M M"
	"M   M"
	"M   M")
    (?P "PPPP "
	"P   P"
	"PPPP "
	"P    "
	"P    "))
  "An alist of (char . (5-strings)) used by show-time-banner.
All 5 strings must be the same length for any given character,
and in top-to-bottom order.")

