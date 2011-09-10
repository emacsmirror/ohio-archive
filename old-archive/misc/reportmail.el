; Path: utkcs2!emory!samsung!cs.utexas.edu!tut.cis.ohio-state.edu!pt.cs.cmu.edu!PROOF.ERGO.CS.CMU.EDU!bcp
; >From: bcp@CS.CMU.EDU (Benjamin Pierce)
; Newsgroups: gnu.emacs
; Subject: Reportmail (was: Mail alert in mode line?)
; Date: 21 Jul 90 03:25:07 GMT
; Organization: Carnegie Mellon University
; 
; 
; Dave Plaut, Mark Leone, and I wrote a bit of code to report incoming
; mail in the mode line.  Installation instructions are included below.
; 
; 	Benli
; 
; 
; 
; 
;; REPORTMAIL: Display time and load in mode line of Emacs.
;; Originally time.el in the emacs distribution.
;; Mods by BCP and DCP to display incoming mail.
;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; To use reportmail, add the following to your .emacs file:
;
;    (load-library "reportmail")
;
;    ;; Edit this list as appropriate
;    (setq display-time-my-addresses
;     '("Benjamin.Pierce" "bcp" "Benjamin Pierce" "\"Benjamin C. Pierce"))
; 
;    ;; By default, mail arrival is reported with a message but no beep  
;    (setq display-time-mail-ring-bell t)
; 
;    (display-time)
; 
;
; In the CMU SCS environment, messages waiting to be read are kept in a 
; file called /usr/spool/mail/<userid>, separated by control-Cs.
; If your environment differs, you will want to modify the values of
; the variables 
;             display-time-incoming-mail-file 
; and/or      display-time-message-separator.
;
;
; The reportmail package has a notion of "junk mail," which can be used to
; reduce the frequency of irritating interruptions by reporting only the
; arrival of messages that seem to be interesting.  If you're on a lot
; of high-volume mailing lists, this can be quite convenient.  To use
; this facility, add something like the following to your .emacs file:
; 
;   ;; The value of this variable is a list of lists, where the first
;   ;; element in each list is the name of a header field and the
;   ;; remaining elements are various prefixes of the value of this
;   ;; header field that signal the junkiness of a message.  
;   (setq display-time-junk-mail-checklist
;     '(("From" "bcp" "Benjamin Pierce" "Benjamin.Pierce"
;               "Mail Delivery Subsystem" "network" "daemon@bartok")
;       ("To" "sml-request" "sml-redistribution-request" 
;        "scheme" "TeXhax-Distribution-list")
;       ("Resent-From" "Benjamin.Pierce")
;       ("Sender" "WRITERS" "Haskell" "Electronic Music Digest" "NEW-LIST")))
;   
;
; If you normally keep your emacs window iconified, reportmail can 
; maintain an xbiff display as well.  The xbiff window will only be 
; highlighted when non-junk mail is waiting to be read.  For example:
;
;    (if window-system-version
;        (setq display-time-use-xbiff t))
;    (setq display-time-xbiff-arg-list '("-update" "30" "-geometry" "+0+0"))
;
;
; There are several other user-customization variables that you may wish
; to modify.  These are documented below.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; HISTORY
;
; 19 Jul 90     Benjamin Pierce (bcp@cs.cmu.edu)
;       Improved user documentation and eliminated known CMU dependencies.
;
; 13 Jul 90	Mark Leone (mleone@cs.cmu.edu)
;	Added display-time-use-xbiff option.  Various layout changes.
;
; 20 May 90     Benjamin Pierce (bcp@cs.cmu.edu)
;       Fixed a bug that occasionally caused fields to be extracted
;       from the wrong buffer.
;
; 14 May 90     Benjamin Pierce (bcp@cs.cmu.edu)
;       Added concept of junk mail and ability to display message
;       recipient in addition to sender and subject.  (Major internal
;       reorganization was needed to implement this cleanly.)
;
; 18 Nov 89     Benjamin Pierce (bcp@cs.cmu.edu)
;       Fixed to work when display-time is called with 
;       global-mode-string not a list
;
; 15 Jan 89	David Plaut (dcp@cs.cmu.edu)
;	Added ability to discard load from displayed string
;
;	To use:	(setq display-time-load nil)
;
;	Added facility for reporting incoming mail (modeled after gosmacs
;	reportmail.ml package written by Benjamin Pierce).


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                       User Variables                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar display-time-announce-mail t
  "*Toggles whether name of mail sender is displayed in mode line.")

(defvar display-time-announce-junk-mail-too nil
  "*When non-NIL, announce incoming junk mail as well as interesting mail")

(defvar display-time-time t
  "*Toggles whether the time is displayed.")

(defvar display-time-load nil
  "*Toggles whether machine load is displayed.")

(defvar display-time-day-and-date nil
  "*Toggles whether day and date are displayed.")

(defvar display-time-mail-ring-bell nil
  "*Toggles whether bell is rung on mail arrival.")

(defvar display-time-my-addresses 
  (list (user-full-name) (user-login-name))
  "*Report the addressee of incoming mail, unless it appears in this list")
;; For example:
;; (setq display-time-my-addresses
;;  '("Benjamin.Pierce" "bcp" "Benjamin Pierce" "\"Benjamin C. Pierce"))

(defvar display-time-junk-mail-checklist nil
  "*A list of lists of strings.  In each sublist, the first component is the
name of a message field and the rest are values that flag a piece of
junk mail.")
;; For example:
;; (setq display-time-junk-mail-checklist
;;  '(("From" "bcp" "Benjamin Pierce" "Benjamin.Pierce"
;;            "Mail Delivery Subsystem" "network" "daemon@bartok")
;;    ("To" "sml-request" "sml-redistribution-request" "computermusic" 
;;     "scheme" "TeXhax-Distribution-list")
;;    ("Resent-From" "Benjamin.Pierce")
;;    ("Sender" "WRITERS" "Haskell" "Electronic Music Digest" "NEW-LIST")))

(defvar display-time-max-from-length 35
  "*Truncate sender name to this length in mail announcements")

(defvar display-time-max-to-length 11
  "*Truncate addressee name to this length in mail announcements")

(defvar display-time-interval 30
  "*Seconds between updates of time in the mode line.  Also used
as interval for checking incoming mail.")

(defvar display-time-incoming-mail-file
  (let ((spool-name (getenv "MAIL")))
    (if (or (null spool-name) (not (file-exists-p spool-name)))
	(let ((user-name (getenv "USER")))
	  (setq spool-name (concat "/usr/spool/mail/" user-name))
	  (if (or (null user-name) (not (file-exists-p spool-name)))
	      (setq spool-name ""))))
    spool-name)
  "User's incoming mail file.  Default is value of environment variable MAIL,
if set;  otherwise /usr/spool/mail/$USER is used.")

(defvar display-time-message-separator "\C-c")

(defvar display-time-use-xbiff nil
  "If set, display-time uses xbiff to announce new mail.")

(defvar display-time-xbiff-arg-list nil
  "List of arguments passed to xbiff.  Useful for setting geometry, etc.")
;;; For example: 
;;; (setq display-time-xbiff-arg-list '("-update" "30" "-geometry" "+0+0"))

(defvar display-time-mail-arrived-file
  (concat "/usr/" (getenv "USER") "/.mail-arrived")
  "New mail announcements saved in this file if xbiff used.  Deleted when 
mail is read.  Xbiff used to monitor existence of this file.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                       Internal Variables                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar display-time-loadst-process nil
  "The process providing time, load, and mail info.")

(defvar display-time-xbiff-process nil
  "The xbiff process used to announce incoming mail.")

(defvar display-time-string nil
  "Time displayed in mode line")

(defvar display-time-mail-buffer-name "*Mail*"
  "Name of buffer used for announcing mail.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                       Macros                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro when (condition &rest body)
  (append (list 'if (list 'not condition) '()) body))

(defmacro unless (condition &rest body)
  (append (list 'if condition '()) body))

(defmacro del-file (filename)
  (list 'if (list 'file-exists-p filename) (list 'delete-file filename)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                       Time Display                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun display-time-kill ()
  "Kill all display-time processes.  Done automatically if display-time
is re-invoked."
  (interactive)
  (if display-time-loadst-process (delete-process display-time-loadst-process))
  (if display-time-xbiff-process (delete-process display-time-xbiff-process))
)


(defun display-time ()
  "Displays current time, date, load level, and incoming mail status in 
mode line of each buffer (if corresponding user variables are set)."
  (interactive)
  (let ((process-connection-type nil))  ; UIUCDCS mod
    (save-excursion
      (display-time-kill)
      (if (or (string-equal "" display-time-incoming-mail-file)
	      (not (file-exists-p display-time-incoming-mail-file)))
	  (error "Display-time-incoming-mail-file not found!"))
      
      (if (not global-mode-string) (setq global-mode-string '("")))
      (if (not (listp global-mode-string))
	  (setq global-mode-string (list global-mode-string "  ")))
      (if (not (memq 'display-time-string global-mode-string))
	  (setq global-mode-string
		(append global-mode-string '(display-time-string))))
      (setq display-time-string "time and load")
      
      (setq display-time-loadst-process
	    (start-process "display-time-loadst" nil
			   "loadst" 
			   "-n" (int-to-string display-time-interval)))
      (process-kill-without-query display-time-loadst-process)
      (set-process-sentinel display-time-loadst-process 
			    'display-time-sentinel)
      (set-process-filter display-time-loadst-process 'display-time-filter)
      
      (if display-time-use-xbiff
	  (progn
	    (del-file display-time-mail-arrived-file)
	    (setq display-time-xbiff-process
		  (apply 'start-process "display-time-xbiff" nil
			 "xbiff" "-file" display-time-mail-arrived-file
			 display-time-xbiff-arg-list))
	    (process-kill-without-query display-time-xbiff-process)
	    (sit-for 1)			; Need time to see if xbiff fails.
	    (if (/= 0 (process-exit-status display-time-xbiff-process))
		(error "Display time: xbiff failed.  Check xbiff-arg-list"))))))
  (display-time-reset-mail-processing))


(defun display-time-sentinel (proc reason)
  (or (eq (process-status proc) 'run)
      (setq display-time-string ""))
  ;; Force mode-line updates
  (save-excursion (set-buffer (other-buffer)))
  (set-buffer-modified-p (buffer-modified-p))
  (sit-for 0))


(defun display-time-filter (proc string)
  ;; Desired data can't need more than the last 30 chars,
  ;; so save time by flushing the rest.
  ;; This way, if we have many different times all collected at once,
  ;; we can discard all but the last few very fast.
  (if (> (length string) 30) (setq string (substring string -30)))
  ;; Now discard all but the very last one.
  (while (and (> (length string) 4)
	      (string-match "[0-9]+:[0-9][0-9].." string 4))
    (setq string (substring string (match-beginning 0))))
  (if (string-match "[^0-9][0-9]+:" string)
      (setq string (substring string 0 (1+ (match-beginning 0)))))
  ;; If we're announcing mail and mail has come, process any new messages
  (when display-time-announce-mail
    (if (string-match "Mail" string)
    	(display-time-process-new-mail)
        (display-time-reset-mail-processing)))
  ;; Format the mode line time display
  (let ((time-string (if (string-match "Mail" string)
			 (if display-time-announce-mail 
			     display-time-mail-modeline
			     "Mail "))))
    (if (and display-time-time (string-match "[0-9]+:[0-9][0-9].." string))
	(setq time-string 
	      (concat time-string
		      (substring string (match-beginning 0) (match-end 0))
		      " ")))
    (if display-time-day-and-date
	(setq time-string
	      (concat time-string
		      (substring (current-time-string) 0 11))))
    (if (and display-time-load (string-match "[0-9]+\\.[0-9][0-9]" string))
	(setq time-string
	      (concat time-string
		      (substring string (match-beginning 0) (match-end 0))
		      " ")))
    ;; Install the new time for display.
    (setq display-time-string time-string)
    ;; Force redisplay of all buffers' mode lines to be considered.
    (save-excursion (set-buffer (other-buffer)))
    (set-buffer-modified-p (buffer-modified-p))
    ;; Do redisplay right now, if no input pending.
    (sit-for 0)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                       Mail processing                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar display-time-mail-who-from ""
  "Short-form name of sender of last piece of interesting unread mail")

(defvar display-time-mail-modeline ""
  "Terse mail announcement (displayed in modeline)")

(defvar display-time-previous-mail-buffer-max 1
  "The length of the mail buffer the last time we looked at it")

(defvar display-time-msg-count 0
  "How many interesting messages have arrived")

(defvar display-time-junk-msg-count 0
  "How many junk messages have arrived")


;; A test procedure for trying out new display-time features
(defun display-time-test ()
  (interactive)
  (display-time-reset-mail-processing)
  (display-time-process-new-mail))

(defun display-time-reset-mail-processing ()
  (let ((mail-buffer (get-buffer display-time-mail-buffer-name)))
    (if mail-buffer (kill-buffer mail-buffer)))
  (if display-time-use-xbiff
      ;; This function is only called when no mail is in the spool.
      ;; Hence we should delete the mail-arrived file.
      (del-file display-time-mail-arrived-file))
  (display-time-reset-mail-processing-vars))

(defun display-time-reset-mail-processing-vars ()
  (setq display-time-msg-count 0)
  (setq display-time-junk-msg-count 0)
  (setq display-time-mail-who-from "Junk mail")
  (setq display-time-previous-mail-buffer-max 1))

(defun display-time-process-new-mail ()
  (let ((mail-buffer (get-buffer display-time-mail-buffer-name))
	start)
    (unless (and mail-buffer (verify-visited-file-modtime mail-buffer))
      (save-window-excursion
	(if mail-buffer (kill-buffer mail-buffer))
	;; Change to pop-to-buffer when we're debugging:
	(set-buffer (find-file-noselect display-time-incoming-mail-file))
	(rename-buffer display-time-mail-buffer-name)
	(if (< display-time-previous-mail-buffer-max (point-max))
	    (progn
	      (goto-char display-time-previous-mail-buffer-max)
	      (if (not (looking-at display-time-message-separator))
		  (display-time-reset-mail-processing-vars)))
	    (display-time-reset-mail-processing-vars))
	(goto-char display-time-previous-mail-buffer-max)
	(if (not (eobp))
	    (forward-char 1))
	(while (not (eobp))
	  (setq start (point))
	  (if (not (search-forward display-time-message-separator nil t))
	      (goto-char (point-max)))
	  (narrow-to-region start (point))
	  (display-time-process-this-message)
	  (goto-char (point-max))
	  (widen))
	(setq display-time-previous-mail-buffer-max (point-max))))))

(defun display-time-process-this-message ()
  ;; Here's where we should check to see whether it's junk mail
  (if (display-time-junk-message)
      (display-time-process-junk-message)  
      (display-time-process-good-message))
  
  ;; Update mode line contents
  (setq display-time-mail-modeline 
	(concat "[" (display-time-format-msg-count) 
		display-time-mail-who-from
		"] "))
  ;; (stop-here (concat "New mode line: " display-time-mail-modeline))
  )

(defun display-time-junk-message ()
  "Check to see whether this message is interesting"
  (let ((checklist display-time-junk-mail-checklist)
	(junk nil))
    (while (and checklist (not junk))
      (if (display-time-member 
	   (display-time-get-field (car (car checklist)))
	   (cdr (car checklist)))
	  (setq junk t)
	  (setq checklist (cdr checklist))))
    junk))

(defun display-time-process-good-message ()
  ;; Update the message counter
  (setq display-time-msg-count (+ display-time-msg-count 1))

  ;; Format components of announcement
  (let* ((subject (display-time-get-field "Subject" ""))
	 (from (display-time-get-field "From" ""))
	 (to (display-time-get-field "To" ""))
         (print-subject (if (string= subject "")
			    ""
			    (concat " (" subject ")")))
	 (print-from (display-time-truncate from display-time-max-from-length))
	 (short-from (display-time-truncate 
		      (display-time-extract-short-addr from) 25))
	 (print-to (if (display-time-member to display-time-my-addresses)
		       ""
		       (display-time-truncate 
			(display-time-extract-short-addr to)
			display-time-max-to-length))))

    ;; Announce message
    (let ((msg (concat 
		   (display-time-format-msg-count)
		   "Mail " 
		   (if (string= print-to "") "" 
		       (concat "to " print-to " "))
		   "from " print-from 
		   print-subject)))
      (if display-time-use-xbiff
	  (save-excursion
	    (let ((tmp-buf (generate-new-buffer "*Tmp*")))
	      (set-buffer tmp-buf)
	      (insert msg)
	      (newline)
	      (append-to-file (point-min) (point-max) 
			      display-time-mail-arrived-file)
	      (kill-buffer tmp-buf))))
      (message "%s" msg))

    (if display-time-mail-ring-bell (ding))
    (sit-for 2)
	
    ;; Update mode line information
    (setq display-time-mail-who-from short-from)))

(defun display-time-process-junk-message ()
  ;; Update the message counter
  (setq display-time-junk-msg-count (+ display-time-junk-msg-count 1))

  ;; Format components of announcement
  (let* ((subject (display-time-get-field "Subject" ""))
	 (from (display-time-get-field "From" ""))
	 (to (display-time-get-field "To" ""))
         (print-subject (if (string= subject "")
			    ""
			    (concat " (" subject ")")))
	 (print-from (display-time-truncate from display-time-max-from-length))
	 (short-from (display-time-truncate 
		      (display-time-extract-short-addr from) 25))
	 (print-to (if (display-time-member to display-time-my-addresses)
		       ""
		       (display-time-truncate 
			(display-time-extract-short-addr to)
			display-time-max-to-length))))

    ;; Announce message
    (when display-time-announce-junk-mail-too
	  (let ((msg (concat 
		      (display-time-format-msg-count)
		      "Junk Mail " 
		      (if (string= print-to "") "" 
			(concat "to " print-to " "))
		      "from " print-from 
		      print-subject)))
	    (if display-time-use-xbiff
		(save-excursion
		  (let ((tmp-buf (generate-new-buffer "*Tmp*")))
		    (set-buffer tmp-buf)
		    (insert msg)
		    (newline)
		    (append-to-file (point-min) (point-max) 
				    display-time-mail-arrived-file)
		    (kill-buffer tmp-buf))))
	    (message "%s" msg)
	    (if display-time-mail-ring-bell (ding))
	    (sit-for 2)))))

(defun display-time-format-msg-count ()
   (if (> (+ display-time-msg-count display-time-junk-msg-count) 1) 
       (concat 
	(int-to-string display-time-msg-count) 
	(if (> display-time-junk-msg-count 0)
	    (concat "(" (int-to-string display-time-junk-msg-count) ")"))
	": ")
       ""))

(defun display-time-get-field (field &optional default)
  (when (not (equal (buffer-name) display-time-mail-buffer-name))
	(beep)
	(message "bcp-time bug: processing buffer %s, not %s"
		 (buffer-name)
		 display-time-mail-buffer-name)
	(sit-for 2))
  (goto-char (point-min))
  (if (re-search-forward (concat "^" field ":[ |\C-i]*") nil t)
    (let ((start (point)))
      (end-of-line)
      (buffer-substring start (point)))
    (or default "<unknown>")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                       Auxilliary Functions                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun display-time-member (e l)
  "Is string E and element of list L?"
  (let ((done nil)
	(result nil))
    (while (not done)
      (cond 
       ((null l) (setq done t))
       ((display-time-is-prefix (car l) e) (setq result l) (setq done t))
       (t (setq l (cdr l)))))
    result))

(defun display-time-truncate (s max)
  (if (and s (>= (length s) max))
      (concat (substring s 0 max) "...")
      s))

(defun display-time-is-prefix (pstr1 str2) 
  "Is PSTR1 a prefix of STR2?"
  (and (<= (length pstr1) (length str2))
      (equal pstr1 (substring str2 0 (length pstr1)))))

(defun display-time-extract-short-addr (long-addr)
  (let ((name "\\([A-Za-z0-9-_+\\. ]+\\)"))
    (if (or 
	 ;; David Plaut <dcp@CS.CMU.EDU>	 -> David Plaut
	 (string-match (concat name "[ |	]+<.+>") long-addr)
	
	 ;; anything (David Plaut) anything	 -> David Plaut
	 (string-match ".+(\\(.+\\)).*" long-addr)
	 
	 ;; plaut%address.bitnet@vma.cc.cmu.edu -> plaut
	 (string-match (concat name "%.+@.+") long-addr)

	 ;; random!uucp!addresses!dcp@uu.relay.net -> dcp
	 (string-match (concat ".*!" name "@.+") long-addr)

	 ;; David.Plaut@CS.CMU.EDU		 -> David.Plaut
	 (string-match (concat name "@.+") long-addr)
	 )
	(substring long-addr (match-beginning 1) (match-end 1))
	long-addr)))


