;;;Subject: fill-paragraph function
;;;Date: 6 Feb 86 08:59:25 GMT
;;;From: Bruce Israel <israel@brillig.umd.edu>

;;;I've recently started using fill-paragraph, and found that it doesn't
;;;preserve indentation unless I set the fill-prefix myself.  What I
;;;wanted was a function that preserved indentation and fill prefix,
;;;without me having to specify it.  fill-individual-paragraphs doesn't
;;;do exactly what I want because it always takes the fill prefix as the
;;;whitespace leading the first line, and that won't handle

;;;>paragraphs that look like this, i.e.
;;;>with a usenet news format reply

;;;    Or even paragraphs that look like this, where
;;;  the person indents the first line of the paragraph
;;;  differently from the rest of the paragraph.

;;;So I wrote the following function which tends to work really nicely as
;;;a current paragraph formatter.  It figures out the common fill prefix,
;;;consisting of non-alphanumeric characters, between all lines in the
;;;paragraph and uses that for justification.  Otherwise its completely
;;;compatible with fill-paragraph (ESC-q).  Enjoy.

;;;Bruce

(defun fill-paragraph-properly (arg)
  "Fill paragraph at or after point, automatically discovering fill-prefix.
Prefix arg means justify as well."
  (interactive "P")
  (save-excursion
    (forward-paragraph)
    (or (bolp) (newline 1))
    (let ((end (point)) st ofill)
      (backward-paragraph)
      (forward-char 1)
      (setq st (point) ofill fill-prefix)
      (if fill-prefix nil
	(while (not (looking-at "[a-zA-Z0-9]")) (forward-character))
	(setq fill-prefix (buffer-substring st (point)))
	(next-line 1)
	(beginning-of-line 1)
	(while (< (point) end)
	  (while (not (looking-at fill-prefix))
	    (setq fill-prefix (substring fill-prefix 0 -1)))
	  (next-line 1)
	  (beginning-of-line 1)))
      (fill-region-as-paragraph st end arg)
      (setq fill-prefix ofill))))


