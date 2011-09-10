;From ark1!uakari.primate.wisc.edu!samsung!cs.utexas.edu!tut.cis.ohio-state.edu!VENERA.ISI.EDU!katz Mon Feb 12 10:18:27 1990
;Article 1127 of gnu.emacs:
;Path: ark1!uakari.primate.wisc.edu!samsung!cs.utexas.edu!tut.cis.ohio-state.edu!VENERA.ISI.EDU!katz
;From katz@VENERA.ISI.EDU
;Newsgroups: gnu.emacs
;Subject: Nifty function to test things
;Message-ID: <9002100125.AA05511@tardis.isi.edu>
;Date: 10 Feb 90 01:25:35 GMT
;Sender: daemon@tut.cis.ohio-state.edu
;Distribution: gnu
;Organization: GNUs Not Usenet
;Lines: 56
;
;
;I wrote the following to make testing new functions easier and have
;found it useful.  Perhaps others will also.
;
;Suppose you have written a lower level function, foo, which is
;supposed to return some value and you want to test it.  However,
;suppose that you can't test it while in lisp-interaction-mode (for
;example, a lower level rmail function, where you must be in
;rmail-mode in order for the function to make sense).  I found myself
;writing test interactive functions which ask the user for arguments, 
;run foo, and then do a (message) to tell what happened.  I finally 
;decided to write a general purpose function.
;
;This is also handy for running built-in functions which happen to be
;non-interactive (such as (current-time-string)).  Also, its handy for
;finding out what interactive functions actually return.
;
;It can run all interactive functions and non-interactive functions
;that take no arguments.  It cannot run non-interactive functions that
;take arguments, since we have no way of knowing what types those 
;arguments should be.
;
;
;You might want to bind it to a key.
;
;				Alan
;
;--------------------------------------------------------

(defvar test-last-test-fun nil
  "The last function we called with test-function")

(defun test-function(fun)
  "For testing, calls the function FUN interactively and reports what it
returns.  Also works on non-interactive functions IF they have no arguments.
If a null line is entered for FUN, calls the function we used when this
was last called."
  (interactive "aTest the function: ")
  (let* ((is-real (> (length (symbol-name fun)) 0))
	 (real-fun (if is-real fun test-last-test-fun))
	 (fname (symbol-name real-fun))
	 (len (if (subrp (symbol-function real-fun)) 0
		(length (car (cdr (symbol-function real-fun)))))))
    (if is-real (setq test-last-test-fun fun))
    (if (commandp real-fun)
	(message "The interactive function %s returned: %s."
		 fname
		 (prin1-to-string (call-interactively real-fun)))
      (if (zerop len)
	  (message "The non-interactive function %s returned: %s."
		   fname
		   (prin1-to-string (apply real-fun nil)))
	(message "Unable to run, %s is non-interactive with %d arg%s."
		 fname
		 len
		 (if (= len 1) "" "s"))))))


