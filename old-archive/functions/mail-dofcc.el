;From ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!swrinde!ucsd!ucbvax!compass.com!worley Tue Apr 10 21:49:59 EDT 1990
;Article 1735 of comp.emacs:
;Path: ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!swrinde!ucsd!ucbvax!compass.com!worley
;>From: worley@compass.com (Dale Worley)
;Newsgroups: comp.emacs
;Subject: FCC in RMAIL
;Message-ID: <9004091559.AA17541@sn1987a.compass.com>
;Date: 9 Apr 90 15:59:11 GMT
;Sender: daemon@ucbvax.BERKELEY.EDU
;Lines: 92
;
;Someone named Robert writes:
;> I'm curious, the FCC (save a copy of message to a file) field seems to
;> do The Wrong Thing in sendmail.  In particular, it seems to assume
;> that the file that it's carbon-copying to is a UNIX mail file, rather
;> than a Babyl file, so if you try to save to a babyl file, it creates
;> garble.  Does anyone have a patch for this (that checks for babyl
;> file, and if so, formats the message properly)?
;
;The problem only arises if the file is already in a buffer, because
;Rmail will convert Unix-style messages appended to an Rmail file into
;Rmail format upon input.  (Make sure that you have applied the patch
;to rmail-output-to-rmail-file I posted previously, because otherwise
;the Rmail 'o' command doesn't reformat Unix-style messages.)  The
;following version of mail-do-fcc handles Rmail files in buffers
;correctly:

(defun mail-do-fcc (header-end)
  (let (fcc-list
	(rmailbuf (current-buffer))
	(tembuf (generate-new-buffer " rmail output"))
	(case-fold-search t))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^FCC:[ \t]*" header-end t)
	(setq fcc-list (cons (buffer-substring (point)
					       (progn
						 (end-of-line)
						 (skip-chars-backward " \t")
						 (point)))
			     fcc-list))
	(delete-region (match-beginning 0)
		       (progn (forward-line 1) (point))))
      (set-buffer tembuf)
      (erase-buffer)
      (insert "\nFrom " (user-login-name) " "
	      (current-time-string) "\n")
      (insert-buffer-substring rmailbuf)
      ;; Make sure messages are separated.
      (goto-char (point-max))
      (insert ?\n)
      (goto-char 2)
      ;; ``Quote'' "^From " as ">From "
      ;;  (note that this isn't really quoting, as there is no requirement
      ;;   that "^[>]+From " be quoted in the same transparent way.)
      (let ((case-fold-search nil))
	(while (search-forward "\nFrom " nil t)
	  (forward-char -5)
	  (insert ?>)))
      (while fcc-list
	(let ((buffer (get-file-buffer (car fcc-list))))
	  (if buffer
	      ;; File is present in a buffer => append to that buffer.
	      (let ((curbuf (current-buffer))
		    (beg (point-min)) (end (point-max)))
		(save-excursion
		  (set-buffer buffer)
		  (if (eq major-mode 'rmail-mode)
		      ;; Append as a message to an RMAIL file
		      (let ((b (point-min))
			    (e (point-max))
			    (buffer-read-only nil))
			(unwind-protect
			    (progn
			      (widen)
			      (goto-char (point-max))
			      ;; This forces RMAIL's message counters to be
			      ;; recomputed when the next RMAIL operation is
			      ;; done on the buffer.
			      ;; See rmail-maybe-set-message-counters.
			      (setq rmail-total-messages nil)
			      (insert "\C-l\n0, unseen,,\n*** EOOH ***\nFrom: "
				      (user-login-name)
				      "\nDate: "
				      (current-time-string)
				      "\n")
			      (insert-buffer-substring rmailbuf)
			      (insert "\n\C-_"))
			  (narrow-to-region b e)))
		    ;; Append to an ordinary file as a Unix mail message
		    (goto-char (point-max))
		    (insert-buffer-substring curbuf beg end))))
	    ;; Else append to the file directly.
	    ;; (It's OK if it is an RMAIL file -- the message will be converted
	    ;; when the file is read in.)
	    (write-region (point-min) (point-max) (car fcc-list) t)))
	(setq fcc-list (cdr fcc-list))))
    (kill-buffer tembuf)))

;Dale Worley		Compass, Inc.			worley@compass.com
;--
;Seen in a net discussion:  It took a lot of work for tofu to become
;politically correct.


