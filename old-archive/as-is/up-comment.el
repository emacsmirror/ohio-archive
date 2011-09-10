;;;From: sra@lcs.mit.edu (Rob Austein)
;;;Subject: foward-comment and backward-comment routines (M-n and M-p)
;;;Date: 30 Jun 89 03:31:20 GMT
;;;Organization: ITS Preservation Society

;;;This has probably already been done, but what the hey.  These two
;;;functions implement one of my favorite missing features from
;;;ITS/Twenex Emacs: ^R Up Comment$ and ^R Down Comment$, which were
;;;bound to M-n and M-p in most major modes in that version of Emacs.

;;;The extra hair involved in cleaning up an existing blank comment is
;;;due to a bug in the existing indent-for-comment code (as of 18.54).
;;;Try doing M-; twice in c-mode and watch the cursor carefully to see
;;;why one can't just match comment-end.

;;;Ta.

;;;--Rob Austein, MIT-LCS

(defun forward-comment (arg)
  "Go to next line and do indent-for-comment there.  With argument,
goes down (or up) that many lines.  If the current line has a comment
that appears to be completely empty, kills it before moving."
  (interactive "p")
  ;; Let indent-for-comment figure out where comment begins
  (indent-for-comment)
  ;; Well, actually, indent-for-comment is a little off and gets a
  ;; little confused by C style /* foo */ comments, so we can't just
  ;; match against comment-end in the obvious fashion.  Kludge city.
  (let ((s (string-match "[^ \t]" comment-end)))
    (setq s (and s (regexp-quote (substring comment-end s))))
    (if (looking-at (concat "[ \t]*" s "$"))
	(kill-comment nil)))
  (forward-line arg)
  (indent-for-comment))

(defun backward-comment (arg)
  "Calls forward-comment with negated argument, for ease of key binding."
  (interactive "p")
  (forward-comment (- arg)))
