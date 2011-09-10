;;; -*- Mode:LISP; Package:ENLIB; Syntax:COMMON-LISP; Base:10 -*-

;Author: Eyvind Ness (eyvind) 
;Date:   Tuesday, July 21 1992 08:44 GMT
;File:   /usr/ife/copma/online/lisp/enlib/src/background-eval.lisp

;;; Definitions for unobtrusive, background evaluation of Lisp forms.
;;; Useful when e.g. evaluating forms over the network.

#-lispm
(in-package "ENLIB")

(export
 '(
   *dev-null*
   with-errors-and-io-trapped
   with-minimum-std-io-syntax
   with-standard-io-syntax-almost-as-in-cltl2
   ))


(defparameter *dev-null*
  #-lispm
  (make-two-way-stream
   (make-concatenated-stream)
   (make-broadcast-stream))
  ;; The lisp machines have a built-in /dev/null which handles
  ;; additional, non-standard operations. Keep it:
  #+lispm #'system:null-stream
  "Conf. Unix /dev/null - `the black hole'")


;;; Lucid (as of 4.0) has no DECLAIM and also lacks these special
;;; variables:
#+lucid
(eval-when (compile load eval)
  (let ((*package* #.(find-package "LISP")))
    (proclaim
     '(special
       *PRINT-LINES*
       *PRINT-MISER-WIDTH*
       *PRINT-PPRINT-DISPATCH*
       *PRINT-RIGHT-MARGIN*
       *READ-EVAL*
       *PRINT-READABLY*
       LUCID::*STANDARD-READTABLE*))))


(defmacro with-standard-io-syntax-almost-as-in-cltl2 (&body body)
  "Just like with-standard-io-syntax, but with a few exceptions to
accommodate the special needs of rpc-hm, and to make up for
inconsitencies between the different CL vendors implementation of
with-standard-io-syntax.

See CLtL2, pp. 566."
  (macrolet ((first-expression (&rest expressions)
	       `',(first expressions)))
    `(let* ((*PACKAGE*
	     (or #.(find-package "USER")
		 #.(find-package "COMMON-LISP-USER")))
	    (*PRINT-ARRAY*	T)
	    (*PRINT-BASE*	10)
	    (*PRINT-CASE*	:UPCASE)
	    (*PRINT-CIRCLE*	NIL)
	    (*PRINT-ESCAPE*	T)
	    (*PRINT-GENSYM*	T)
	    (*PRINT-LENGTH*	NIL)
	    (*PRINT-LEVEL*	NIL)
	    (*print-lines*      nil)
	    (*print-miser-width* nil)
	    (*print-pprint-dispatch* nil)
	    (*PRINT-PRETTY*	NIL)
	    (*PRINT-RADIX*	NIL)
	    ;; don't signal errors when printing unreadable objects - the
	    ;; rpc-hm client may want it all the same:
	    (*PRINT-READABLY*	nil)
	    (*print-right-margin* nil)
	    (*READ-BASE*	10)
	    (*READ-DEFAULT-FLOAT-FORMAT*	'SINGLE-FLOAT)
	    (*READ-EVAL*	T)
	    (*READ-SUPPRESS*	NIL)
	    (*READTABLE*
	     ,(first-expression
	       #+genera 	si:*common-lisp-readtable*
	       #+explorer 	sys:common-lisp-readtable
	       #+lucid		lucid::*standard-readtable*
	       *readtable*))
	    #+genera
	    (scl::*print-structure-contents* t)
	    #+explorer
	    (ticl::*PRINT-STRUCTURE*	t)
	    #+lucid
	    (lucid::*PRINT-STRUCTURE*	t)
	    #+explorer
	    (sys::*READER-SYMBOL-SUBSTITUTIONS*
	     sys::*COMMON-LISP-SYMBOL-SUBSTITUTIONS*)
	    )
      . ,body)))


(defmacro with-minimum-std-io-syntax (&body body)
  "If there is a `with-standard-io-syntax' macro around, use it,
otherwise set up the things we need for doing reliable file-io in our
context.
See `with-standard-io-syntax-almost-as-in-cltl2 and CLtL2, pp. 566"
  (let ((extra-let-bindings
	 '((*PACKAGE* (or #.(find-package "USER")
			  #.(find-package "COMMON-LISP-USER")))
	   (*PRINT-READABLY* t))))
    (if (fboundp 'with-standard-io-syntax)
	`(with-standard-io-syntax (let* ,extra-let-bindings . ,body))
	;; else DIY:
	`(with-standard-io-syntax-almost-as-in-cltl2
	  (let* ,extra-let-bindings . ,body)))))


(defmacro with-errors-and-io-trapped (&body forms)
  "
During the execution of FORMS, errors are trapped, the standard CL
IO-streams, *standard-input*, *query-io*, *debug-io*, and *terminal-io*,
are lexically rebound to *dev-null*, a stream that immediately supplies
EOF on input and discards all output, while output to any of the streams
*standard-output*, *error-output*, and *trace-output* is trapped and
returned as a component of the return value produced by the execution of
this macro.

This macro uses a customized version of the CLtL2 macro
`with-standard-io-syntax' to set up default, sensible values for
printing control, so that the reader may be able to re-read the printed
output. The standard version cannot be used directly here, since in this
case wee need a more flexible scheme to allow e.g. #<...> objects to be
transferred back to the client, which should have the ultimate control
of what to do with the object.
See with-standard-io-syntax-almost-as-in-cltl2.

The return value is always a list of 3 values:

  The first value is T if the evaluation of FORMS did not signal an
error condition, NIL otherwise.

  The second value is the actual value produced by FORMS within an
implicit PROGN, wrapped into a string with `prin1-to-string' to simplify
parsing at the client end of a communication channel. If an error was
signaled during the execution of FORMS, an error message is returned.
This message is also wrapped by `prin1-to-string' to make unwrapping
independent of the actual outcome.

  The third value returned is a string containing the output to any of
the standard CLtL2 output streams, *standard-output*, *error-output*,
and *trace-output*, if any, produced by FORMS.

Note that if the evaluation of FORMS returns multiple values, only the
first value is preserved by this macro.

Use MULTIPLE-VALUE-LIST or MULTIPLE-VALUE-BIND if you need to retrieve
multiple-values produced by FORMS."
  
;;; Hint to find candidate symbol-packages:
;;; (mapcar #'symbol-package (find-all-symbols "BREAK"))
  
  (macrolet ((first-expression (&rest expressions)
	       `',(first expressions)))
    (let ((tag (gensym))
	  (trapping-output-stream (gensym))
	  (default-break-ret-val (gensym)))
      `(with-standard-io-syntax-almost-as-in-cltl2
	(catch ',tag
	  (let* ((,trapping-output-stream
		  (make-string-output-stream))
		 (,default-break-ret-val
		     '(nil "\"Break not meaningful in this context\"" ""))
		 (*standard-input* *dev-null*)
		 (*standard-output* ,trapping-output-stream)
		 (*error-output* ,trapping-output-stream)
		 (*query-io* *dev-null*)
		 (*debug-io* *dev-null*)
		 (*terminal-io* *dev-null*)
		 (*trace-output* ,trapping-output-stream)
		 (,(first-expression
		    #+lispm conditions:*debugger-hook*
		    #+lucid lcl:*debugger-hook*
		    *debugger-hook*)
		  #'(lambda (condition old-dbg-hook)
		      (declare (ignore old-dbg-hook))
		      (throw ',tag
			(list
			 nil
			 (prin1-to-string
			  (format
			   nil
			   "Attempt to call debugger trapped: ~a" condition))
			 "")))))
	    (,(first-expression
	       #+explorer			;CLEH is a nickname of CONDITIONS
	       conditions:handler-bind
	       ;; if you want to use ticl:condition-bind instead, remember to
	       ;; change `conditions:condition' to `lisp:error' below.
	       #+genera
	       scl:condition-bind		;not cltl2 compatible (as of 8.0)
	       #+lucid
	       lcl:handler-bind
	       ;; otherwise, let's just hope there is a CLtL2 definition around:
	       handler-bind)
	      ((,(first-expression
		  #+explorer
		  conditions:condition	;the most general condition type
		  #+genera
		  cl:error		;same - in the old error system
		  #+lucid
		  lcl:condition
		  condition)
		 #'(lambda (condition)
		     (unless
			 ;; decline to handle conditions of type WARNING
			 (typep
			  condition
			  ',(first-expression
			     #+lispm
			     (or conditions:warning system:warning)
			     #+lucid
			     lcl:warning
			     warning))
		       (throw ',tag
			 (list
			  nil
			  (prin1-to-string (format nil "~a" condition))
			  ""))))))
	      (flet (#+genera
		     (cl:break (&rest args)
		       (declare (ignore args))
		       (throw ',tag ,default-break-ret-val))
		     #+genera
		     (zl:break (&rest args)
		       (declare (ignore args))
		       (throw ',tag ,default-break-ret-val))
		     #+(or explorer lucid)
		     (lisp:break (&rest args)
		       (declare (ignore args))
		       (throw ',tag ,default-break-ret-val))
		     #+explorer
		     (conditions:break (&rest args)
		       (declare (ignore args))
		       (throw ',tag ,default-break-ret-val))
		     #-(or explorer genera lucid)
		     (break (&rest args)
		       (declare (ignore args))
		       (throw ',tag ,default-break-ret-val))
;;; Get rid of the function responsible for this:
;;;		     
;;; Remote lisp server barfed:
;;;
;;; The first argument to = was of the wrong type.
;;; The function expected a number.
;;;
;;; [This is due to a bug in #'fs:keyboard-query: It does (send
;;; *query-io* ':tyi) and always expect a character to be returned,
;;; while #'sys:null-stream will typically return NIL when confronted
;;; with a ':tyi-message]
		     #+explorer
		     (fs::keyboard-query (&rest args)
		       (declare (ignore args))
		       (throw ',tag
			 '(nil "\"KEYBOARD-QUERY not meaningful in this context\"" "")))
;;; Genera gives the following, more descriptive error message
;;;
;;; Remote lisp server barfed:
;;;
;;; The object SYS:NULL-STREAM received a :RESTORE-INPUT-BUFFER message, which went unclaimed.
;;; The rest of the message was (NIL T).
;;; The message is handled by the flavors SI:INTERACTIVE-STREAM, ZWEI:MINI-IE-STREAM, and ZWEI:ZWEI.
		     #+lucid
		     ;; Block random calls to lcl:quit
		     (lcl:quit (&rest args)
		       (declare (ignore args))
		       (throw ',tag
			 '(nil "\"LCL:QUIT not meaningful in this context\"" "")))
		     )
		(list
		 t
		 (prin1-to-string (progn ,@forms))
		 (get-output-stream-string ,trapping-output-stream))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test cases:
;;;
;;; (progn
;;;   (format *standard-output* "~&stdout~%")
;;;   (format *error-output* "~&stderr~%")
;;;   (format *debug-io* "~&dbg-out~%")
;;;   (format *query-io* "~&q-io~%")
;;;   (format *terminal-io* "~&t-io~%")
;;;   (format *trace-output* "~&t-out~%")
;;;   (break)
;;;   )
;;; (dbg:debugger-top-level (conditions:make-condition 'conditions:simple-error
;;; :format-string "haha") nil nil nil)
;;; (conditions:invoke-debugger (conditions:make-condition 'conditions:simple-error
;;; :format-string "haha"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Performance data:
;;; (time (dotimes (i 100) (enlib:with-errors-and-io-trapped (progn))))
;;;
;;; Explorer II:
;;; Evaluation of (DOTIMES (I 100)
;;;   (WITH-ERRORS-AND-IO-TRAPPED (PROGN))) took 2.121644 Seconds of elapsed time,
;;; including 0.004865 seconds of paging time for 30 faults, Consed 15156 words.
;;; NIL
;;;
;;; HPs400/Lucid 4.0:
;;; > (time (dotimes (i 100) (lib:with-errors-and-io-trapped (progn))))
;;; Elapsed Real Time = 1.07 seconds
;;; Total Run Time    = 1.06 seconds
;;; User Run Time     = 1.06 seconds
;;; System Run Time   = 0.00 seconds
;;; Dynamic Bytes Consed   =          0
;;; Ephemeral Bytes Consed =    114,464
;;; NIL
;;; > 
;;; HPs700/Lucid 3.0:
;;; > (time (dotimes (i 100) (lib:with-errors-and-io-trapped (progn))))
;;; Elapsed Real Time = 0.14 seconds
;;; Total Run Time    = 0.14 seconds
;;; User Run Time     = 0.14 seconds
;;; System Run Time   = 0.00 seconds
;;; Dynamic Bytes Consed   =          0
;;; Ephemeral Bytes Consed =    116,024
;;; NIL
;;; > 

;;; -------------------------------------------------------------------
;;; Hello, Emacs - here are some hints for you (in case this file is
;;; accidentally put in the wrong (non-lisp) mode):
;;;
;;; Local Variables:
;;; write-file-hooks: (update-std-header update-sccs-string clpu-make-export-list)
;;; comment-start: ";"
;;; comment-end: ""
;;; comment-start-skip: ";+ *"
;;; End:
;;; -------------------------------------------------------------------
