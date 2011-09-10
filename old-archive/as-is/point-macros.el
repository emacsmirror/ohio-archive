;;;Date: 11 Jun 89 23:35:05 GMT
;;;From: Ray Nickson <murtoa.cs.mu.oz.au!munnari.oz.au!comp.vuw.ac.nz!news@uunet.uu.net>
;;;Subject: setting a variable to another point (was:  A little function for USENET admins)
;;;Organization: Dept. of Computer Science, Victoria Uni. of Wellington, NZ.

;;;In article <TALE.89Jun6172831@imagine.pawl.rpi.edu> tale@pawl.rpi.edu (David C Lawrence) writes:

;;;   As an aside, how many elisp hackers have wanted to do
;;;   (setq beg (beginning-of-line)) to get the same results as
;;;   (progn (beginning-of-line) (setq beg (point)))?

;;;   I find similar examples in lots of elisp code and it just seems so
;;;   distinctly un-lispy to have a function like (end-of-line) which never
;;;   returns anything besides nil and has only a side-effect.  It seems
;;;   that most movement commands which don't already have meaningful return
;;;   values would be improved slightly if they returned point instead.

;;;My fave macros are:

(defmacro point-at (&rest forms)
  "Evaluate list of forms, and return the value of point after evaluation."
  (` (progn (,@ forms)
	    (,@ '((point))))))

(defmacro point-if (&rest forms)
  "Evaluate list of forms, and return the value of point after evaluation.
Restores point to its original value before returning."
  (` (save-excursion (,@ forms)
		     (,@ '((point))))))

;;;my elisp code is full of things like
;;;(buffer-substring (current-buffer)
;;;		  (point-if (beginning-of-line nil))
;;;		  (point-if (end-of-line nil))).

;;;I agree, though, that a function that always returns nil is just begging
;;;to be `featureized'.

;;;   Dave

;;;--
;;;nickson@comp.vuw.ac.nz       ...!uunet!vuwcomp!nickson           + 64 4 753358
