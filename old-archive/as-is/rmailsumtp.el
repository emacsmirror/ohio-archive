To: unix-emacs@BBN.COM
Date: 23 May 89 19:15:52 GMT
From: "Raymond A. Schnitzler" <needle1!ras@bellcore.bellcore.com>
Sender: arpa-unix-emacs-request@BBN.COM
Subject: rmailsum addition: rmail-summary-by-topic
Reply-To: "Raymond A. Schnitzler" <ras%needle1.uucp@BBN.COM>
Organization: Bellcore, Red Bank, NJ
Source-Info:  From (or Sender) name not authenticated.


Here are two defuns (which you can add to your rmailsum file) to
provide a topic-based summarizer (rmail-summary-by-topic).  It is
analogous to other rmail-summary-by-* functions, but will look
anywhere (in the header or in the whole message) for matches.  I find
it useful for tracking down and linking messages that might not even
have the same subject.  It is also useful for collecting related
messages that might be best matched some by recipient, some by
subject, etc.
;
;I put this on M-C-t (ESC ^t).
;
;Enjoy.

--------------------
;; rmail-summary-by-topic
;; 1989 R.A. Schnitzler
;; Distribution rules as for GNUemacs.

;(define-key rmail-mode-map "" 'rmail-summary-by-topic).

(defun rmail-summary-by-topic (subject &optional whole-message)
  "Display a summary of all messages with the given SUBJECT.
Normally checks the Subject field of headers;
but if WHOLE-MESSAGE is non-nil (prefix arg given), 
 look in the whole message.
SUBJECT is a string of names (regexps) separated by commas."
  (interactive "sTopics to summarize by: \nP")
  (rmail-new-summary
   (concat "about " subject)
   'rmail-message-subject-p
   (mail-comma-list-regexp subject) whole-message))

(defun rmail-message-subject-p (msg subject &optional whole-message)
  (save-restriction
    (goto-char (rmail-msgbeg msg))
    (search-forward "\n*** EOOH ***\n")
    (narrow-to-region
     (point)
     (progn (search-forward (if whole-message "" "\n\n")) (point)))
    (goto-char (point-min))
    (if whole-message (re-search-forward subject nil t)
      (string-match subject (or (mail-fetch-field "Subject") "")) )))

