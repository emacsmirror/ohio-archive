;From arpa-unix-emacs-request@CHIPS.BBN.COM Sun Apr  9 09:52:14 1989
;Received: from chips by CHIPS.BBN.COM id aa00778; 9 Apr 89 10:36 EDT
;Received: from BBN.COM by CHIPS.BBN.COM id aa00774; 9 Apr 89 10:34 EDT
;Received: from USENET by bbn.com with netnews
;	for arpa-unix-emacs@bbn.com (unix-emacs@bbn.com);
;	contact usenet@bbn.com if you have questions.
;To: unix-emacs@bbn.com
;Date: 6 Apr 89 09:21:00 GMT
;From: ukma!uflorida!novavax!hcx1!hcx2!tom@husc6.harvard.edu
;Sender: arpa-unix-emacs-request@bbn.com
;Subject: Re: Question regarding next-line
;Message-Id: <94600023@hcx2>
;References: <8363@csli.STANFORD.EDU>
;Source-Info:  From (or Sender) name not authenticated.
;Status: RO
;
;
;Or do what I do, go to the lisp source and hack your own version that
;does exactly what you want (that's what emacs is all about isn't it?).
;Here is the version I use - it will add at most one newline if there
;is not already one at the end of the buffer:

; Replace the system next-line with one that inserts a max of one
; newline
(defun next-line (arg)
  "Move cursor vertically down ARG lines.
If there is no character in the target line exactly under the current column,
the cursor is positioned after the character in that line which spans this
column, or at the end of the line if it is not long enough.
If you are on the last line in a buffer and there is no newline, one
is automatically inserted.

The command \\[set-goal-column] can be used to create
a semipermanent goal column to which this command always moves.
Then it does not try to move vertically.

If you are thinking of using this in a Lisp program, consider
using `forward-line' instead.  It is usually easier to use
and more reliable (no dependence on goal column, etc.)."
  (interactive "p")
  (save-excursion
     (forward-line 1)
     (if (eq (preceding-char) ?\n)
        nil
        (insert ?\n)
     )
  )
  (line-move arg)
)

;P.S. This was hacked from the version in the gnuemacs 18.52 source.

;=====================================================================
;    usenet: tahorsley@ssd.harris.com  USMail: Tom Horsley
;compuserve: 76505,364                         511 Kingbird Circle
;     genie: T.HORSLEY                         Delray Beach, FL  33444
;======================== Aging: Just say no! ========================
