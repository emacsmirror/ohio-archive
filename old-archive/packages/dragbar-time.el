;; LCD Archive Entry:
;; dragbar-time|Travis J.I. Corcoran|tjic@icd.teradyne.com|
;; Displays time and date in an x-windows drag bar|
;; 30-Apr-95|0.9|~/packages/dragbar-time.el|
;;
;;------------------------------------------------------------
;;                    dragbar-time.el
;;
;;                    Travis J.I. Corcoran        30 Apr 95
;;
;;                    bug reports to tjic@icd.teradyne.com
;;------------------------------------------------------------
;;
;;   What this package does:
;;
;;        Displays time and date in an x-windows drag bar, instead of
;;        on the mode line.  This is a Good Thing (tm) bc the drag bar
;;        is as wide as the entire emacs window, never gets
;;        subdivided, and never gets clogged up w per-buffer info.
;;
;;   How to install:
;;
;;        add to your .emacs:
;;            	(display-time)           ;  make the process active
;;              (require 'dragbar-time)  ;  move the time to frame!
;;
;;   How to use:
;;
;;        Sit back and watch the dragbar rell you the time, mail status, etc.
;;
;;   How to customize:
;;
;;       (1) to supress new-mail beeping:
;;		        (setq db-beep-for-mail nil)
;;       (2) to change the data displayed in the drag bar
;;              (setq db-format-string ....)
;;
;;   KNOWN BUGS:
;;       
;;
;;   REVISION HISTORY:
;;
;;   30 Apr 95: TJICorcoran <tjic@icd.teradyne.com>    : more cleanup
;;   29 Apr 95: Jari Aalto  <jaalto.tre.tele.nokia.fi> : packaged, cleanup
;;          94: TJICorcoran <tjic@icd.teradyne.com>    : wrote basic code



; ----------------------------------------------------------------------
;

      ;;  first make sure we have the time display package loaded.
      ;;  Shame, it doesn't seem to have provide...

	  ;;  this might be autoloaded...and thus we don't need this

(if (not (fboundp 'display-time)) (load "time"))


;;; ----------------------------------------------------------------------
;;;

(defvar db-no-mail-string " ----"
  "*String to be printed to dragbar when no mail is pending")

(defvar db-yes-mail-string " ####"
  "*String to be printed to dragbar when mail is pending")

(defvar db-beep-for-mail t
  "*If non-nil, beep when mail arrives")

(defvar db-supress-day-of-week t
  "*Meaningful only if display-time-day-and-date is set.
If non-nil, supress day of week")

(defvar db-mail-waiting nil
  "Internal use variable.  if mail is pending - non-nil")

(defvar db-old-mail-flag nil)
(defvar db-old-time-string nil)
(defvar db-old-frame-string nil)

(defvar db-format-string
  '(concat
	db-old-frame-string     "  ;  "
	display-time-string  "  ;  "
	mail-flag
	)
  ""
)


;;; ----------------------------------------------------------------------
;;;

(defun display-time-filter (proc string)
  (let ((time (current-time-string))
	(load
	 (condition-case ()
	     (if (zerop (car (load-average))) ""
	       (let ((str (format " %03d" (car (load-average)))))
		 (concat (substring str 0 -2) "." (substring str -2))))
	   (error "")))
	(mail-spool-file (or display-time-mail-file
			     (getenv "MAIL")
			     (concat rmail-spool-directory
				     (user-login-name))))
	hour am-pm-flag mail-flag)

    (setq hour (read (substring time 11 13)))

    (if display-time-24hr-format nil	;ignore this
      (setq am-pm-flag (if (>= hour 12) "pm" "am"))
      (if (> hour 12)
	  (setq hour (- hour 12))
	(if (= hour 0)
	    (setq hour 12)))
      (setq am-pm-flag ""))

    (setq mail-flag
	  (if (and (or (null display-time-server-down-time)
		       ;; If have been down for 20 min, try again.
		       (> (- (nth 1 (current-time))
			     display-time-server-down-time)
			  1200))
		   (let ((start-time (current-time)))
		     (prog1
			 (display-time-file-nonempty-p mail-spool-file)
		       (if (> (- (nth 1 (current-time)) (nth 1 start-time))
			      20)
			   ;; Record that mail file is not accessible.
			   (setq display-time-server-down-time
				 (nth 1 (current-time)))
			 ;; Record that mail file is accessible.
			 (setq display-time-server-down-time nil))
		       )))
	      ;; from here down is my code -------------------
	      db-yes-mail-string
	    db-no-mail-string))

	;;;  ------------------------------ from here down is my code ---------
	;;;  ---------------------------------- VVVVVVVVVVVVVV ----------------

    (if (equal mail-flag db-yes-mail-string)
        (if db-mail-waiting
			(setq db-mail-waiting nil)
		  (setq db-mail-waiting t)
								; only beep if we're transitioning
								; from no-mail to mail
		  (if (and db-beep-for-mail (not (string= db-old-mail-flag mail-flag)))
			  (beep))))

    (setq display-time-string
	  (concat (format "%d" hour) (substring time 13 16)
		  am-pm-flag
		  load ))

    ;; Append the date if desired.
    (if display-time-day-and-date
	(setq display-time-string
	      (concat (substring time 0 11) display-time-string)))

    (db-dragbar-update mail-flag)			;<<<

    (setq display-time-string nil)
    )

	;;;  ---------------------------------- ^^^^^^^^^^^^ ----------------
	;;;  ------------------------------ from here up is my code ---------

  (run-hooks 'display-time-hook)
  ;;   Force redisplay of all buffers' mode lines to be considered.
  (save-excursion (set-buffer (other-buffer)))
  (set-buffer-modified-p (buffer-modified-p))
  ;;   Do redisplay right now, if no input pending.
  (sit-for 0))



(defun db-dragbar-update (mail-flag)

  "Updates the frame's status line."
  ;; only update the bar when needed,
  ;; no matter how often this function
  ;; is called
  (if (or (not (string= db-old-mail-flag mail-flag))
		  (not (string= db-old-time-string display-time-string))
		  )
      (progn
		;;  Save the contents of frame name, eg. host name
		(if db-old-frame-string nil		;already saved
		  (setq
		   db-old-frame-string
		   (cdr (assq 'name  (frame-parameters (selected-frame))))
		   ))
		(setq db-old-mail-flag mail-flag
			  db-old-time-string display-time-string)
		(modify-frame-parameters
		 (selected-frame)
		 (list
		  (cons 'name
				(eval db-format-string)))))
	;; ............... end progn ..........
	))





(provide 'dragbar-time)

;;; end of file

