;From: jcgs@jung.harlqn.uucp (John Sturdy)
;Newsgroups: comp.emacs
;Subject: more formatting stuff
;Message-ID: <JCGS.89Sep11084258@jung.harlqn.uucp>
;Date: 11 Sep 89 07:42:58 GMT
;Organization: Harlequin Ltd, Cambridge, England
;Lines: 42
;
;
;Once you've got your comment paragraphs filled, with this function you
;can give them tidy box borders:
;
(defun comment-straighten-right-edge ()
  "Align the right-hand edges of a comment
of the form (example in C):
/* Here is the first line */
/* of a multiple          */
/* comment in your code.  */
The alignment is made on the longest line of a sequence of
consecutive comment lines, starting with the current line."  
  (interactive)
  (save-excursion
    (beginning-of-line 1)
    (let ((start-of-area (point))
	  (c-e-len (length comment-end)))
      (end-of-line 1)
      (let ((end-of-line (point))
	    (longest-line -1))
	(beginning-of-line 1)
	(while (search-forward comment-end end-of-line 1)
	  (setq longest-line (max longest-line (current-column)))
	  (end-of-line 2)
	  (setq end-of-line (point))
	  (beginning-of-line 1))
	(beginning-of-line 0)
	(setq longest-line (- longest-line c-e-len))
	(while (>= (point) start-of-area)
	  (search-forward comment-end end-of-line 1)
	  (backward-char c-e-len)
	  (while (< (current-column) longest-line)
	    (insert " "))
	  (beginning-of-line 0))))))
;--
;__John            When asked to attend a court case, Father Moses took with him
;          a leaking jug of water. Asked about it, he said: "You ask me to judge
;               the faults of another, while mine run out like water behind me."
;
;                jcgs@uk.co.harlqn (UK notation) jcgs@harlqn.co.uk (most places)
;    ...!mcvax!ukc!harlqn!jcgs (uucp - really has more stages, but ukc knows us)
;John Sturdy                                            Telephone +44-223-872522
;                      Harlequin Ltd, Barrington Hall, Barrington, Cambridge, UK
