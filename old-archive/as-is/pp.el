;;;Date: Mon, 12 Sep 88 17:24:36 EDT
;;;From: Root Boy Jim <rbj@nav.icst.nbs.gov>
;;;Subject: What's elisp missing?

;;;Why, a pretty printer, of course!

;;;I have often wished to see the definitions of functions, especially my
;;;own, but no standard way exists, possibly because most functions end up
;;;getting byte compiled, and you can always edit the text in a buffer anyway.

;;;Nevertheless, I wrote this little gem. It pretty prints just about anything
;;;that can be a function definition. Interactively, it requires a symbol
;;;with a function definition, but if called from elisp it will look in
;;;the symbol's value cell if its function definition is void. This can be
;;;used to print keymaps as in (pp 'global-map).

;;;Given a function `f', it leaves the output in buffer `*f*' in a form
;;;suitable to the lisp reader that will redefine the function in most
;;;cases.  With a prefix argument, you can be a voyeur. As an example of
;;;its output, I am posting the output of running it upon itself. Enjoy.

(defun pp (function &optional verbose) "Pretty Print FUNCTION"
  (interactive "aPretty Print: 
P")
  (let*
      ((name
	(symbol-name function))
       (buffer
	(concat "*" name "*")))
    (with-output-to-temp-buffer buffer
      (select-window
       (display-buffer buffer))
      (set-buffer buffer)
      (emacs-lisp-mode)
      (erase-buffer)
      (prin1
       (if
	   (fboundp function)
	   (symbol-function function)
	 (symbol-value function)))
      (goto-char 1)
      (if verbose
	  (sit-for 0))
      (cond
       ((looking-at "(macro lambda")
	(replace-match "(defmacro ")
	(insert name)
	(forward-sexp 1))
       ((looking-at "(lambda")
	(replace-match "(defun ")
	(insert name)
	(forward-sexp 1))
       (t
	(insert-parentheses 1)
	(insert "fset '" name " ")))
      (while
	  (not
	   (eobp))
	(if verbose
	    (sit-for 0))
	(cond
	 ((looking-at "[][()]")
	  (skip-chars-forward "][()"))
	 ((looking-at " [([]")
	  (delete-char 1)
	  (insert 10))
	 (t
	  (forward-sexp))))
      (goto-char 1)
      (indent-sexp))))

;;;	(Root Boy) Jim Cottrell	<rbj@icst-cmr.arpa>
;;;	National Institute of Standards and Technology
;;;	Flamer's Hotline: (301) 975-5688
;;;	The opinions expressed are solely my own
;;;	and do not reflect NIST policy or agreement
;;;	Careful with that VAX Eugene!
