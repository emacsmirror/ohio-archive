; From: phs@lifia.imag.fr (Philippe Schnoebelen)
; Subject: Dired gadget (+ general suggestion)
; Date: 13 Nov 90 21:03:38 GMT
; Organization: Lab. LIFIA -- Univ. Grenoble -- France
; 
; Here is a little gadget for Dired:
; 
(defun dired-delete-this-file ()
  "In dired, delete the file named on this line."
  (interactive)
  (let ((buffer-read-only nil)
	(fname (dired-get-filename)))
    (if (not (y-or-n-p (format "Delete file %s " fname)))
	(message "OK, I won't.")
	;; else, do it !
	(delete-file fname)
	(delete-region (progn (beginning-of-line) (point))
		       (progn (forward-line 1) (point)))
	(message "Done"))))

I've put:
	   (define-key dired-mode-map "\C-d" 'dired-delete-this-file)

in my dired-mode-hook. (Admittedly, the choice of \C-d is poor taste. Feel
free to suggest improvements.)

Now, you can remove a file from your directory just by hitting \C-d when
you're pointing at it in Dired. 


I very much prefer this kind of interactive behavior over the usual (in
GnuEmacs) way of marking for deletion and then deleting. A similar case
could be presented for Buffer-List, or VM-summary, ...  It would be nice if
this were added _in_a_consistent_way_ in all relevant Emacs packages. What
do others think ?
--
Philippe SCHNOEBELEN,  LIFIA,  46 Av Felix VIALLET,  38000 Grenoble,  FRANCE
						     email: phs@lifia.imag.fr
"Algebraic symbols are used when you do not know what you are talking about."


