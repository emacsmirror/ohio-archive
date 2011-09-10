;;;Received: from bbnccy.arpa by BBNCC5.ARPA id a029710; 11 Oct 85 16:14 EDT
;;;Received: from BBNCC5.ARPA by BBNCCY ; 11 Oct 85 15:54:25 EDT
;;;Received: by bbncc5.ARPA (4.12/4.7)
;;;	id AA29269; Fri, 11 Oct 85 15:55:24 edt
;;;Relay-Version: version B 2.10.2 9/18/84; site bbncc5.UUCP
;;;Posting-Version: version B 2.10.1 6/24/83; site bu-cs.UUCP
;;;Path: bbncc5!bbnccv!harvard!bu-cs!root
;;;From: Barry Shein <root%bu-cs.UUCP@BBNCC5.ARPA>
;;;Newsgroups: net.emacs
;;;Subject: Critical GNU emacs prog!
;;;Message-Id: <697@bu-cs.UUCP>
;;;Date: 6 Oct 85 01:32:54 GMT
;;;Date-Received: 6 Oct 85 05:19:44 GMT
;;;Organization: Boston Univ Comp. Sci.
;;;Lines: 43
;;;Apparently-To: jr
;;;
;;;Ok people, like, I'm *real* busy these days, but fortunately I found the
;;;time to write this thing cuz I know you couldn't live another day without
;;;it....
;;;
;;;	-Barry Shein, Boston University
;;;
;;;-------fold and cut here--------------
;;;
;;; Canonical GNU copyright conditions apply
;;;   TWINKLE (c) Barry Shein 1985
;;; Though I admit it's not very original
;;;
(defun twinkle ()
  "TWINKLE creates and displays a twinkling buffer
typing any char stops and returns to previous buffer"
  (interactive)
  (let* ((curbuf (current-buffer))
	 (newbuf (get-buffer-create "*Twinkle*"))
	 (height (1- (window-height)))
	 (width  (1- (screen-width)))
	 (i 0)
	 (init-string (make-string width 32))  ;;; that is, a line of blanks
	 (mask 255))                           ;;; random can return negative
    (switch-to-buffer newbuf)
    (delete-other-windows)
    (erase-buffer)
    (while (< i height)                        ;;; fill window w/ blanks
      (princ init-string newbuf)
      (princ "\n" newbuf)
      (setq i (1+ i)))
                                               ;;; try for a little giggle
    (message "Twinkle Twinkle little star...       [hit space to stop]")
    (while (not (input-pending-p))             ;;; randomly (un)display *'s
      (goto-char 1)
      (forward-char (% (logand (random) mask) width))
      (next-line (% (logand (random) mask) height))
      (delete-char (if (bolp) 1 -1))
      (if (zerop (logand (random) 1)) (princ "*" newbuf)
	(princ " " newbuf))(sit-for 0))
    (read-char)                                ;;; swallow up stop char
    (switch-to-buffer curbuf)
    (message                                   ;;; try for guilt
     "You'll sit and watch this idiocy but you won't sing along, sigh...")))
