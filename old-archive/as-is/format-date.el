;;;Date: 9 Sep 88 22:02:39 GMT
;;;From: Damian Cugley <otter!pdc@hplabs.hp.com>
;;;Subject: Re: insert time/date and signature
;;;Organization: Hewlett-Packard Laboratories, Bristol, UK.

;;;Wayne Mesard:						<mesard@bbn.com>
;;;> GNU Emacs has a (current-time-string) function. [...]
;;;>  (defun insert-stamp ()
;;;>    "Inserts timestamp and userid as: Day MMM DD HH:MM:SS YYYY -- USER NAME"
;;;>    (interactive)
;;;>    (insert (current-time-string) " -- " (user-full-name) )
;;;>  )
;;;> 
;;;> This has the disadvantage of outputting the time in that bizarre
;;;> ctime(3) format.  But it would be trivial to add some "substring" calls
;;;> to get it to do the right thing.

;;;Funny you should mention that.  I was bored this morining and one of the
;;;things I did was write some funcs to reformat Notes (that is, netnews)
;;;files, which includes a date-reforming function (quoted at the end of
;;;this article).  I was just reading comp.emacs in order to ask something
;;;about it!

;;;This was designed to be invoked just after inserting the date in the
;;;`Notes format', and has been altered in the obvious way to fit the ctime
;;;dates (or any similar format).  Then we might define:

;;;(defun timestamp ()
;;;  "Documentation goes here.  Be creative."
;;;  (interactive) ;of course
;;;  (insert (user-full-name) ", " (current-time-string))
;;;  (format-date))

;;;Example:
;;;	Damian Cugley, 9 Sep 1988


;;;pdc
;;;--
;;;  /-------------------\/-------------------------\/------------------------\ 
;;;  | Damian Cugley =@= || pdc@hplb.lb.hp.co.uk    || ...!mcvax!ukc!hplb!pdc |  
;;;  | HPLabs Bristol UK || pdc%otter@hplabs.HP.COM ||   ...!hplabs!otter!pdc |  
;;;  \-------------------/\-------------------------/\------------------------/  

;;;;;;;;;;;;;;;;;;;;;;;;;SNIP HERE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	Reformat the date

(defconst silly-date-regexp
	"\\([a-zA-Z]+\\) +\\([a-zA-Z]+\\) +\\([0-9]?[0-9]\\) +\\([0-9:]+\\) +\\([0-9]+\\)"
	"This is a regexp used to translate dates from the ctime fromat
to one I happen to like.  The five parameters are the day of week, month
day-of-month, time-of-day and year.")

(defconst my-date-format
	"\\3 \\2 \\5"
	"How I like the date to appear.  The sequence `\\DIGIT' is translated
as in query-replace-regexp; see silly-date-format for the meaning of these
`parameters'.")

;; Version for Notes:
;;(defconst silly-date-regexp
;;  "\\([0-9]?[0-9]:[0-9][0-9]\\)[\t ]+[apAP][mM][ \n]+\\([a-zA-Z]+\\)[\t ]+\\([0-9]+\\),?[ \t]*\\([0-9]+\\)\\>"
;;  "A regexp that represents the time-and-date as produced by many
;;American programs.")

;;(defconst my-date-format
;;  "\\3 \\2 \\4"
;;  "*How I like the date formatted - `\\\\DIGIT' (where DIGIT is an
;;integer from 1 to 9) represents `parameters' (that is, \\(...\\)
;;groupings from silly-date-regexp.")

(defun format-date ()
  "Reformat the date string before point on this line  into a format I like."
  (interactive)
  (if (re-search-backward
       silly-date-regexp
       (save-excursion (beginning-of-line) (point))
       t)
      (replace-match my-date-format)
    (message "?? There doesn't seem to be a date on this line.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
