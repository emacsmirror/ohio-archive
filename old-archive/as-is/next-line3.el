;From arpa-unix-emacs-request@CHIPS.BBN.COM Tue Apr  4 23:13:03 1989
;Received: from chips by CHIPS.BBN.COM id aa18421; 4 Apr 89 23:58 EDT
;Received: from BBN.COM by CHIPS.BBN.COM id aa18417; 4 Apr 89 23:57 EDT
;Received: from USENET by bbn.com with netnews
;	for arpa-unix-emacs@bbn.com (unix-emacs@bbn.com);
;	contact usenet@bbn.com if you have questions.
;To: unix-emacs@bbn.com
;Date: 5 Apr 89 04:00:24 GMT
;From: Atul Khanna <bbn.com!akhanna@bbn.com>
;Sender: arpa-unix-emacs-request@bbn.com
;Subject: Re: forward-line (was Re: Question regarding next-line)
;Reply-To: Atul Khanna <akhanna%alexander.bbn.com.bbn.com@CHIPS.BBN.COM>
;Message-Id: <38268@bbn.COM>
;References: <38147@bbn.COM>, <4266@omepd.UUCP>, <4437@psuvax1.cs.psu.edu>
;Organization: Bolt Beranek and Newman Inc., Cambridge MA
;Source-Info:  From (or Sender) name not authenticated.
;Status: RO
;
;>
;>Except that forward-line does some extra magic, like keeping track
;>of the (temporary) goal column, which we _do_ like.  It would be nice
;>to be able to select the behavior you like by setting a flag,
;>*auto-extend-buffer-forward*, say.
;
;I'm sure you meant next-line above.  Anyway, I use the following
;definition of next-line (apparently from Bill Wohler
;<wohler@spam.istc.sri.com>), which does not put extra lines at the end
;of the buffer, despite saying that it does.
;
(defun next-line (arg)
  "Move cursor vertically down ARG lines.
If there is no character in the target line exactly under the current column,
the cursor is positioned after the character in that line which spans this
column, or at the end of the line if it is not long enough.
If there is no line in the buffer after this one,
a newline character is inserted to create a line
and the cursor moves to that line.

The command \\[set-goal-column] can be used to create
a semipermanent goal column to which this command always moves.
Then it does not try to move vertically.

If you are thinking of using this in a Lisp program, consider
using `forward-line' instead.  It is usually easier to use
and more reliable (no dependence on goal column, etc.)."
  (interactive "p")
  (if (= arg 1)
      (let ((opoint (point)))
	(forward-line 1)
	(if (not (eq (preceding-char) ?\n))
	    (insert ?\n)
	  (if (eq opoint (point))
	      (progn
		(beep)
		(message "End of buffer"))
	    (goto-char opoint)
	    (line-move arg))))
    (line-move arg))
  nil)

;--------------------------------------------------------------------------
;Atul C. Khanna <akhanna@alexander.bbn.com>
;BBN Communications Corporation
;150 CambridgePark Drive
;Cambridge, MA 02140
;
;UUCP:   {harvard,rutgers,uunet,...}!bbn.com!akhanna
;Tel: (617) 873 2531
