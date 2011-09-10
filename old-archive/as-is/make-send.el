;From POP2-Server@k30b Thu Oct 20 07:12:15 1988
;Received: from pizza by PIZZA.BBN.COM id aa12998; 20 Oct 88 6:22 EDT
;Received: from BBN.COM by PIZZA.BBN.COM id aa12994; 20 Oct 88 6:20 EDT
;Received: from USENET by bbn.com with netnews
;	for arpa-unix-emacs@bbn.com (unix-emacs@bbn.com);
;	contact usenet@bbn.com if you have questions.
;To: unix-emacs@bbn.com
;Date: 17 Oct 88 19:26:46 GMT
;From: Dale Worley <compass.UUCP!worley%eddie.mit.edu.uucp@bbn.com>
;Sender: arpa-unix-emacs-request@bbn.com
;Subject: More features for RMAIL
;Message-Id: <8810171926.AA04449@galaxy.compass.com>
;Source-Info:  From (or Sender) name not authenticated.
;
;This code adds a command to RMAIL: make-send-via-send-and-exit.  This
;command makes copies of any To: header line and changes it to a
;Sent-via: line.  The advantage of this is that the header of the
;outgoing message contains the exact address the message was sent by,
;and can make it much easier to figure out which addresses get through
;to their destination and which don't.
;
; insert sent-via lines
(load "sendmail" nil t)
(setq mail-mode-hook
      '(lambda () (progn
		   (define-key
		     mail-mode-map
		     "\C-c\C-n"
		     'make-send-via-send-and-exit))))

(defun make-send-via-send-and-exit ()
  "Run  make-send-via  on this buffer (thus duplicating all To: lines as
Sent-via: lines), and then run  mail-send-and-exit  (thus mailing the
message)."
  (interactive)
  (make-sent-via)
  (mail-send-and-exit))

(defun make-sent-via ()
  "Turn copies of all To: lines into Sent-via: lines.  This is to be run on
a mail buffer before  sendmail-send-it ."
  (goto-char (point-min))
  ; find the header-separator
  (search-forward (concat "\n" mail-header-separator "\n"))
  ; put a marker at the end of the header
  (setq end (set-marker (make-marker) (point)))
  (goto-char (point-min))
  ; search for the To: lines and make Sent-via: lines from them
  (let ((case-fold-search t)
	b to-line)
    ; search for the next To: line
    (while (progn (setq case-fold-search t)
		  (re-search-forward "^\\(to\\):" end t))
      (setq b (point))
      ; find all the continuations of this header line (ignoring empty lines)
      (re-search-forward "^[^ \t\n]" end 'move)
      (if (/= (point) end)
	  (backward-char 1))
      ; copy the To: line and put Sent-via: before it
      (setq to-line (buffer-substring b (point)))
      (insert-before-markers "Sent-via:" to-line))))
