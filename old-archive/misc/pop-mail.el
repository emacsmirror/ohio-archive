; Path: dg-rtp!rock.concert.net!mcnc!stanford.edu!bu.edu!dimacs.rutgers.edu!mips!zaphod.mps.ohio-state.edu!wuarchive!uunet!mcsun!unido!infbs!neitzel@infbs.uucp
; From: neitzel@infbs.uucp (Martin Neitzel)
; Newsgroups: gnu.emacs.sources
; Subject: pop-mail.el for 18.57
; Date: 6 Jun 91 16:57:22 GMT
; 
; I just converted my version of good ol' pop-mail to work with 18.57.
; (For those who wanna know: pop-mail sits on top of display-time and will
; automatically pop up an RMAIL buffer and "g"et newly arriving mail.)
; 
; It is basically a drop in replacement for one function in time.el.  You
; could replace the function in standard time.el or overload it from a new
; file.  For the latter, put something like this into your .emacs:
; 
; (display-time)
; (load "pop-mail") ;; to get the extented functionality.
; 
; Thanks go to wolfgang@mgm.mit.edu, who came up with the code originally.
; 
; 								Martin

;; Copyright (C) 1985, 1986, 1987 Free Software Foundation, Inc.
;; Read the GNU COPYING file for the full details.
;; 9/17/87 wolfgang@mgm.mit.edu

(defvar pop-mail t "*If t display new mail in a pop up mail buffer.
	files listed in rmail-primary-inbox-list are watched")

(defun display-time-filter (proc string)
  (let ((time (current-time-string))
	(load (condition-case ()
		  (if (zerop (car (load-average))) ""
		    (format "%03d" (car (load-average))))
		(error "")))
	(mail-spool-file (or display-time-mail-file
			     (getenv "MAIL")
			     (concat rmail-spool-directory
				     (or (getenv "LOGNAME")
					 (getenv "USER")
					 (user-login-name)))))
	hour pm)
    (setq hour (read (substring time 11 13)))
    (setq pm (>= hour 12))
    (if (> hour 12)
	(setq hour (- hour 12))
      (if (= hour 0)
	  (setq hour 12)))
    (setq display-time-string
	  (concat (format "%d" hour) (substring time 13 16)
		  (if pm "pm " "am ")
		  (if (string= load "")
		      ""
		    (concat (substring load 0 -2) "." (substring load -2)))
		  (if (and (file-exists-p mail-spool-file)
			   ;; file not empty?
			   (> (nth 7 (file-attributes mail-spool-file)) 0))
		      " Mail"
		    "")))
    ;; Append the date if desired.
    (if display-time-day-and-date
	(setq display-time-string
	      (concat (substring time 0 11) display-time-string))))
  ;; Force redisplay of all buffers' mode lines to be considered.
  (save-excursion (set-buffer (other-buffer)))
  (set-buffer-modified-p (buffer-modified-p))
  ;; Do redisplay right now, if no input pending.
  (sit-for 0)
  (if pop-mail
      (let ((list (or rmail-primary-inbox-list
		      '("/usr/spool/mail/$USER")))
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
