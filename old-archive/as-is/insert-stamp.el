;;;Date: 8 Sep 88 20:22:52 GMT
;;;From: Wayne Mesard <bbn.com!mesard@bbn.com>
;;;Subject: Re: insert time/date and signature

;;;From article <8809081924.AA09732@symbol.sunecd.com>, by cdr@symbol.UUCP (Constantine Rasmussen - Sun ECD):
;;;> (defun insert-date-and-time (&optional use-long-form)
;;;>   "Args: (&OPTIONAL USE-LONG-FORM)
;;;> Returns value from call-process.
;;;> Insert the current date and time at point.  Use of a prefix argument
;;;> will invoke long form, ie, \"Tue, Jul 12, 1988, 02:17:44 PM\".
;;;> Always ends with a newline."
;;;> 
;;;>   (interactive "P")
;;;>   (call-process "date"
;;;> 		nil t nil
;;;> 		(if use-long-form
;;;> 		    "+%a, %h %d, 19%y, %r"
;;;> 		    "+%D, %H:%M")))
;;;> 
;;;> (global-set-key "t" 'insert-date-and-time)
;;;> 
;;;> 

;;;GNU Emacs has a (current-time-string) function.  (Like the spaghetti
;;;sauce commercials: "It's in der!")  So something like this might do the
;;;trick, without a subprocess:

 (defun insert-stamp ()
   "Inserts timestamp and userid as: Day MMM DD HH:MM:SS YYYY -- USER NAME"
   (interactive)
   (insert (current-time-string) " -- " (user-full-name) )
 )

;;;This has the disadvantage of outputting the time in that bizarre
;;;ctime(3) format.  But it would be trivial to add some "substring" calls
;;;to get it to do the right thing.

;;;-- 
;;;unsigned *Wayne_Mesard();  "There are tons of people who are well adjusted
;;;MESARD@BBN.COM              that have zits."
;;;BBN, Cambridge, MA                                    -EN

