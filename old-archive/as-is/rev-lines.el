;To: unix-emacs@bbn.com
;Date: 7 Feb 89 23:32:44 GMT
;From: Nate Hess <oliveb!intelca!mipos3!nate@ames.arc.nasa.gov>
;Subject: Re: `format' can do leading-zero padding (was Re: line numbers in emacs)
;
;In article <4110@omepd.UUCP>, merlyn@intelob (Randal L. Schwartz @ Stonehenge) writes:
;>  (insert (format "%07d:" num))
;>
;>to get the leading-zero-padded numbers needed above.  Cute, eh?
;
;
;Cute, indeed.  In fact, here is some code I wrote a year or so ago that
;you might find useful that takes advantage of it:


;;
;; A cute function to reverse the ordering of lines.
;; Dedicated to Victoria.  (So I'll even put comments in :-)#
;;
;; NOTE:  This function will die a horrible death if an attempt is made
;; to reverse a region containing more than 999999 lines.  Sport Death!
;;
(defun reverse-lines-in-region (start end)
  "Reverses the order of the lines from START to END."
  (interactive "r")
  (let ((count 1))
    (save-excursion
      ;
      ; Position point at the start of the last line of the region to be
      ; sorted.  
      ;
      (goto-char end)
      (beginning-of-line)
      (setq end (point))
      ;
      ; We then visit each line in the region, stopping long enough to
      ; say "Hello, eh?!" and insert a formatted number at the beginning
      ; of the line.  The formatting of the number merely consists of
      ; padding with zeros on the left.
      ;
      ; NOTE:  Check out the first 'and' condition down below.  Yeah, I
      ; know, "Pretty fugly," you're saying to yourself.  That nasty
      ; side-effect inside the testing.  Feel free, gentle reader, to
      ; clean up the code; please send diffs to me.  :->#
      ;
      (message "Calculating index numbers...")
      (while (and (= 0 (forward-line -1))
		  (>= (point) start))
	(insert (format "%06d" count))
	(setq count (1+ count)))
    ;
    ; We have the index numbers at the front of every line in the
    ; region, so now we can go ahead and sort, but we can't do it until
    ; we know the *current* end of the region; you'll remember (from
    ; yesterday's class) that all those insertions we performed above
    ; will increase the location of the Region's End.  (Great movie!)
    ; So, add to END the number of characters inserted into the buffer.
    ;
    (setq end (+ end (* 6 (1- count))))
    (sort-lines nil start end)
    ;
    ; Trash the index numbers at the start of every line with an
    ; intensely cute rectangle delete.  "Pretty cool, huh?"
    ;
    (delete-rectangle start
		      (progn
			(goto-char end)
			(forward-line -1)
			(+ 6 (point)))))))


;Have fun!
;--woodstock
;--
;	   "What I like is when you're looking and thinking and looking
;	   and thinking...and suddenly you wake up."   - Hobbes
;
;woodstock@hobbes.intel.com   ...!{decwrl|hplabs!oliveb|amd}!intelca!mipos3!nate 
;
;