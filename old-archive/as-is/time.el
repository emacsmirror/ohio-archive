;To: unix-emacs@bbn.com
;Date: 30 Dec 88 15:50:54 GMT
;From: Kyle Jones <rti!talos!kjones@mcnc.org>
;Subject: `time.el' revisited
;
;Attached is an enhanced version of the GNU Emacs Lisp library `time.el'.
;It may be used as a drop-in replacement for the existing `time.el'
;file.  Remember to byte-compile it.
;
;Features added:
;* setting display-time-use-sysline non-nil causes Berkeley's sysline(1)
;  to be used instead of loadst.  Code was added to deal rationally
;  with sysline's long `new mail' message.
;* setting auto-save-time-interval to a numeric value N causes buffers
;  to be auto-saved every N seconds (actual granularity depends on the
;  display-time-interval).
;* setting display-time-echo-area non-nil causes the time to be
;  displayed in the echo area instead of the mode line.  The clock's
;  output does not interfere with normal use of the minibuffer.
;* display-time always uses a pipe for IPC.
;
;kyle jones   <kyle@odu.edu>   ...!uunet!talos!kjones
;-----
;; Display time and load in mode line or echo area of Emacs.
;; Copyright (C) 1985, 1986, 1987, 1988 Free Software Foundation, Inc.

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

;; display-time-use-sysline variable, Kyle Jones, December 1988
;; Echo area code added by Kyle Jones, August 1988
;; Auto-save and sysline code added by Kyle Jones, July 1987
;; Made to always use a pipe by Kyle Jones, July 1987

(defvar display-time-interval 60
  "*Seconds between updates of time in the mode line.")

(defvar display-time-echo-area nil
  "*Non-nil means display time in the echo area instead of the mode line.
Note that using the minibuffer clears the time display until the next
time the clock updates.")

(defvar display-time-use-sysline nil
  "*Non-nil means use Berkeley's sysline(1) as the clock process.")

(defvar auto-save-time-interval nil
  "*Number of seconds between auto-save'ing buffers that need it.")

(defvar auto-save-timer 0
  "Number of seconds since auto-save-timer last expired")

(defconst display-time-process nil)
(defconst display-time-string nil)
(defconst display-time-truncate (screen-width))

(defun display-time ()
  "Display current time and load level in mode line of each buffer,
or the echo area, if the value of display-time-echoarea in non-nil.
Updates automatically every minute.
If display-time-day-and-date is non-nil, the current day and date
are displayed as well."
  (interactive)
  (let (process-connection-type
	(live (and display-time-process
		   (eq (process-status display-time-process) 'run))))
    (if (not live)
	(save-excursion
	  (if display-time-process
	      (delete-process display-time-process))
	  (or display-time-echo-area
	      global-mode-string
	      (setq global-mode-string '("")))
	  (or display-time-echo-area
	      (memq 'display-time-string global-mode-string)
	      (setq global-mode-string
		    (append global-mode-string '(display-time-string))))
	  ;; (auto-save-timer) is called the first time before any time has
	  ;; elapsed, so we set the timer to account for this
	  (setq auto-save-timer (- display-time-interval))
	  (setq display-time-string "time and load")
	  (setq display-time-process
		(if display-time-use-sysline
		    (start-process "display-time" nil
				   "sysline" (if display-time-echo-area
						 "-perh" "-perhl")
				   (concat "+" display-time-interval))
		  (start-process "display-time" nil
				 "loadst" "-n"
				 (int-to-string display-time-interval))))
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

(defun display-time-filter (proc string)
  ;; First do the necessary auto-save's
  (if (and (boundp 'auto-save-time-interval) (numberp auto-save-time-interval))
      (auto-save-timer))
  ;; Desired data can't need more than the last display-time-truncate chars,
  ;; so save time by flushing the rest.
  ;; This way, if we have many different times all collected at once,
  ;; we can discard all but the last few very fast.
  (if (> (length string) display-time-truncate)
      (setq string (substring string (- display-time-truncate))))
  ;; Now discard all but the very last one.
  (while (and (> (length string) 4)
	      (string-match "[0-9]+:[0-9][0-9].." string 4))
    (setq string (substring string (match-beginning 0))))
  (if (string-match "[^0-9][0-9]+:" string)
      (setq string (substring string 0 (1+ (match-beginning 0)))))
  ;; for sysline
  (if (string-match "\\(Mail\\) from \\([^ ]+\\)" string)
      (setq string (substring string 0 (if display-time-echo-area
					   (match-end 2)
					 (match-end 1)))))
  ;; Append the date if desired.
  (if display-time-day-and-date
      (setq string (concat (substring (current-time-string) 0 11) string)))
  ;; Install the new time for display.
  (cond ((and display-time-echo-area (zerop (minibuffer-depth)))
	 (save-excursion
	   (set-buffer (window-buffer (minibuffer-window)))
	   (erase-buffer)
	   (indent-to (- (1- (screen-width)) (length string)))
	   (insert string)))
	(t 
	 (setq display-time-string string)
	 ;; Force redisplay of all buffers' mode lines to be considered.
	 (save-excursion (set-buffer (other-buffer)))
	 (set-buffer-modified-p (buffer-modified-p))))
  ;; Do redisplay right now, if no input pending.
  (sit-for 0))

(defun auto-save-timer ()
  (setq auto-save-timer (+ auto-save-timer display-time-interval))
  (cond
   ((>= auto-save-timer auto-save-time-interval)
    (do-auto-save)
    (setq auto-save-timer (- auto-save-timer auto-save-time-interval)))))

