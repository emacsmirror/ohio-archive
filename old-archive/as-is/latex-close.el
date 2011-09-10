;;;Date: 3 Mar 88 17:18:08 GMT
;;;From: Ashwin Ram <YALE.ARPA!Ram-Ashwin@EDDIE.MIT.EDU>
;;;Organization: Computer Science, Yale University, New Haven, CT 06520-2158
;;;Subject: Re: Closing LaTeX blocks

;;;In article <8802230342.AA12720@ATHENA.CS.YALE.EDU>, ram-ashwin@YALE (Ashwin Ram) writes:
;;;> Does anyone have code that recognizes "\begin{foo} ... \end{foo}" pairs in a
;;;> LaTeX file as a "balanced pair"?  In particular, I'm looking for something
;;;> that would validate a buffer, indicate a mismatch (just as for different
;;;> kinds of parens), and insert the correct \end{...} expression upon hitting a
;;;> key, depending on the current \begin{...} expression that you're in?

;;;I got several useful replies to this (thanks, guys!), including a couple of
;;;fairly elaborate latex-modes.  I also got some requests to forward my final
;;;solution, so here goes.  (I should add that I got a couple of nicer solutions
;;;than this in response to my request.  This is just the simplest and least
;;;elaborate fix that works, which you can install in a couple of minutes, so I
;;;decided to share it with the net.)

;;;The easiest solution is to redefine TeX-close-LaTeX-block (in the file
;;;lisp/tex-mode.el).  Just replace it with the following function (which is
;;;already bound to C-c C-f by default).  [I suggest that this be replaced in
;;;the GNU Emacs distribution since it's a lot nicer than the old function.]

;;;----------------------------------------------------------------------------
;; Replacement for TeX-close-LaTeX-block in lisp/tex-mode.el.
;; Ashwin Ram, 2/22/88.
(defun TeX-close-LaTeX-block (&optional validate)
   "Inserts an \\end{...} to match corresponding \\begin{...}.
If optional argument VALIDATE is t, doesn't insert anything, only validates."
   (interactive "*")
   (let ((fail-point (point))
         (nesting '())
         (done nil))         
      (end-of-line)
      (while (not done)
         (if (re-search-backward "\\\\\\(begin\\|end\\){\\([^}\n]*\\)}" (point-min) t)
             (let ((which (buffer-substring (match-beginning 1) (match-end 1)))
                   (text (buffer-substring (match-beginning 2) (match-end 2))))
                (if (equal which "end")
                    (setq nesting (cons text nesting))
                    (if (null nesting)
                        (let ((indentation (current-column)))
                           (goto-char fail-point)
                           (if validate
                               (error "\\begin{%s} is never ended" text)
                               (progn
                                  (beginning-of-line)
                                  (if (not (looking-at "^[ \t]*\n"))
                                      (progn (end-of-line) (insert "\n")))
                                  (indent-to indentation)
                                  (insert "\\end{" text "}\n")))
                           (setq done t))
                        (if (equal text (car nesting))
                            (setq nesting (cdr nesting))
                            (error "\\begin{%s} ended by \\end{%s}" text (car nesting))))))
             (progn
                (goto-char fail-point)
                (if (not validate) (message "No unmatched \\begin{...} to end here"))
                (setq done t))))))
----------------------------------------------------------------------------

To do begin-end checking in addition to {} checking, I changed the following
function by adding a couple of lines to call the above.  This is less than
ideal, I suppose, but it works.  The lines are initialled.

----------------------------------------------------------------------------
(defun TeX-validate-paragraph (start end)
  (condition-case ()
      (save-excursion
	(save-restriction
	  (narrow-to-region start end)
          (goto-char end)           ;; Added, AR 2/22/88.
          (TeX-close-LaTeX-block t) ;; Added, AR 2/22/88.
	  (goto-char start)
	  (forward-sexp (- end start))
	  t))
    (error nil)))
;;;----------------------------------------------------------------------------
