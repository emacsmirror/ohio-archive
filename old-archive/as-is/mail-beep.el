;To: unix-emacs@BBN.COM
;Date: 2 Jun 89 19:45:39 GMT
;From: John Robinson <jr@BBN.COM>
;Sender: arpa-unix-emacs-request@BBN.COM
;Subject: Re: mail beep (take 3)
;Reply-To: John Robinson <jr@BBN.COM>
;References: <GHH.89May15093238@clarity.princeton.edu>, <40158@bbn.COM>
;Organization: BBN Systems and Technologies Corporation, Cambridge MA
;Source-Info:  From (or Sender) name not authenticated.
;
;In article <40158@bbn.COM>, I, jr@bbn (John Robinson) write:
;>In article <GHH.89May15093238@clarity.princeton.edu>, ghh@clarity (Gilbert Harman) writes:
;>>Is there a way to get (display-time) or some other process
;>>to beep when mail first arrives, in addition to displaying
;>>"Mail" in the mode-line?
;> ...
;>(defun display-time-filter (proc string)
;
;... and so forth.  The defun made use of the save-match-data macro,
;newly arrived to the list, to protect the (global, sadly) match-data
;during this asynchronous function's execution.  I failed to notice
;that the distributed code in time.el for display-time-filter already
;trashes match-data in the lines prior to my modification.  Here's a
;newer defun, with match-data protected properly, and the
;save-match-data macro to boot in case you missed it before.
;--------
(require 'cl)
(defmacro save-match-data (&rest body)
  "Execute the BODY forms, restoring the global value of the match data."
  (let ((original (gensym)))
    (list
     'let (list (list original '(match-data)))
     (list 'unwind-protect
	   (cons 'progn body)
	   (list 'store-match-data original)))))
(put 'save-match-data 'lisp-indent-hook 0)

(defun display-time-filter (proc string)
  ;; Desired data can't need more than the last 30 chars,
  ;; so save time by flushing the rest.
  ;; This way, if we have many different times all collected at once,
  ;; we can discard all but the last few very fast.
  (save-match-data
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
    ;; Check if mail just arrived, and ring the bell
    (if (and (string-match "Mail" string)
	     (not (string-match "Mail" display-time-string)))
	(ding t)))
  ;; Install the new time for display.
  (setq display-time-string string)
  ;; Force redisplay of all buffers' mode lines to be considered.
  (save-excursion (set-buffer (other-buffer)))
  (set-buffer-modified-p (buffer-modified-p))
  ;; Do redisplay right now, if no input pending.
  (sit-for 0))

;--
;/jr, nee John Robinson   What a waste it is to lose one's mind--or not
;jr@bbn.com or bbn!jr      to have a mind.  How true that is. -Dan Quayle

