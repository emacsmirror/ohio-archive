;To: unix-emacs@bbn.com
;Date: 17 Feb 89 10:43:34 GMT
;From: Silver <njin!aramis.rutgers.edu!athos.rutgers.edu!gaynor@princeton.edu>
;Subject: Re: Copy From Above?
;
;Hmph, it appears my original posting didn't make it out, or I used reply
;instead of follow-up, or whatever.  C'est la vie.

;; (define-key global-map "\C-\\" 'insert-from-above-command)

(defun insert-from-above-command (n)
"Insert the character at the same column from the previous non-blank line at
the point.  Prefix argument specifies maximum number of characters to copy, as
less will be copied if that previous line is too short."
  (interactive "p")
  (let* ((cc (current-column))
	 pt)
    (insert (save-excursion
	      (beginning-of-line)
	      (and (bobp) (error "Beginning of buffer"))
	      (skip-chars-backward "\ \t\n")
	      ;; Characters which take up more than 1 column of display are
	      ;; tricky.
	      (if (<= cc (move-to-column cc))
		(buffer-substring
		  (setq pt (point))
		  (progn (skip-chars-forward "^\n" (+ (point) n))
			 ;; 1+ to grab the newline if not n chars
			 (if (< (point) (+ pt n))
			   (1+ (point))
			   (point)))))))))

;Regards, [Ag] gaynor@rutgers.edu

