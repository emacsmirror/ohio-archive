;;; $Header: /home/user3/miles/src/elisp/RCS/setf.el,v 1.20 1992/04/21 19:17:28 miles Exp $
;;; ----------------------------------------------------------------
;;; setf.el -- Setf for elisp
;;; Copyright (C) April 1992, Miles Bader <miles@cogsci.ed.ac.uk>
;;; ----------------------------------------------------------------
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;; ----------------------------------------------------------------
;;;
;;; Implements most of the common-lisp setf functions (however, e.g.,
;;; lists are returned instead of multiple-values, and only a single store
;;; variable is ever used, since we don't have multiple-values).
;;;
;;; The major functions defined are:
;;;   setf, psetf, shiftf, rotatef, push, pop, incf, decf, swapf (macros)
;;;   defsetf (macro)
;;;   define-setf-method (macro)
;;;   define-modify-macro (macro)
;;;   get-setf-method
;;;
;;; See the end of the file for examples, and "Common Lisp: The Language"
;;; (Steele) for more complete documentation.
;;;
;;; Some non-standard macros defined:
;;;   setf-update-form makes some common setf definitions easier to write.
;;;   with-setf-method is just syntactic sugar for binding the result of
;;;     get-setf-method.
;;;   defsetf-and-return is like the short-form of defsetf, but arranges for
;;;	the new-value to be returned; this is for things who's update-function
;;;     has the right argument order, but doesn't return the right value.
;;;
;;; Because of the let-optimizer, the resulting code is pretty reasonable, e.g.
;;;   (rotatef (car x) (cdr x))  ;; swap the car and cdr of x
;;;  ==> (let* ((G676 (car x))) (setcar x (cdr x)) (setcdr x G676) nil)
;;;
;;; **NOTE** Setf depends on two non-standard elisp packages:
;;;   * Jamie Zawinski's optimizing byte compiler, for the eval-and-compile
;;;	special-form.
;;;   * Rick Sladkey's backquote package (or rather, it doesn't work with the
;;;     standard backquote implementation that comes with emacs, since that has
;;;     all sorts of bugs).
;;;
;;; It also uses gensym.el and letopt.el, which should be provided.
;;;
;;; An example of using define-setf-method is provided in plist.el.
;;;
;;; Setf has to autoload things itself, before macroexpanding, since a form's
;;; setf definition and the setf definition of its macroexpansion may be
;;; different.
;;;
;;; Setf will autoload anything who's fifth arg to autoload was non-NIL,
;;; indicating a macro; we're going to try macroexpand next anyway, so this
;;; doesn't do anything horrible.  If there isn't really any macroexpansion,
;;; for the given function, only a setf definition, you should probably make
;;; this some indicative non-NIL value like 'SETF.  If something has both a
;;; setf definition and normal function value, maybe something like 'ALSO-SETF.
;;;

(provide 'setf)

(require 'backquote)
(require 'gensym)
(require 'letopt)			; optimize let bindings

;;; This is needed for setf definition forms to be properly evaluated at
;;; compile time.  It comes from Jamie Zawinski's byte-compiler.
(autoload 'eval-and-compile "bytecomp" nil nil t)

;;; The proper environment to use while compiling.  It's nil otherwise,
;;; and so doesn't cause any problems.
(defvar byte-compile-macro-environment)

;;; ----------------------------------------------------------------

(defun %setf-gensyms (num)
  (if (zerop num)
      nil
      (cons (gensym) (%setf-gensyms (1- num)))))

(defun %setf-zip (x y)
  (and x
       (cons (list (car x) (car y))
	     (%setf-zip (cdr x) (cdr y)))))

(defun has-setf-method-p (place-form)
  "Returns T if PLACE-FORM has a setf-method.

See get-setf-method for more information about setf-methods."
  (or (and (symbolp place-form) form)
      (and (not (atom place-form))
	   (let ((fun (car place-form)))
	     (or (get fun 'setf-update-fun)
		 (get fun 'setf-method))))))

(defun %get-setf-simple-update-fun (place-form)
  (cond ((symbolp place-form)
	 'setq)
	(t
	 (get (car place-form) 'setf-update-fun))))

;;; Try autoloading the function in form (see comment at start of file for
;;; explanation of why setf autoloads things itself).
(defun %setf-try-autoloading (form original-form)
  (and (listp form)
       (atom (car form))
       (fboundp (car form))
       (let ((func (symbol-function (car form))))
	 (and (listp func)
	      (eq (car func) 'autoload)
	      (nth 4 func)
	      (if (or (not (load (nth 1 func) t))
		      (eq (symbol-function (car form)) func))
		  (error "Autoload of %s failed" (car form))
		  (%setfable-form-or-lose form original-form))))))

(defun %setf-try-macroexpanding (form original-form)
  (let ((expanded-form
	 (macroexpand form byte-compile-macro-environment)))
    (and (not (equal form expanded-form))
	 (%setfable-form-or-lose expanded-form
				 (or original-form form)))))

(defun %setfable-form-or-lose (form &optional original-form)
  (if (has-setf-method-p form)
      form
      (or (%setf-try-autoloading form original-form)
	  (%setf-try-macroexpanding form original-form)
	  (error "Form has no setf method: %s" (or original-form form)))))

(defmacro with-setf-method (vars place-form &rest body)
  "(with-setf-method VARLIST PLACE-FORM BODY...) binds the variables in
VARLIST to the various components of the setf-method for PLACE-FORM and
evaluates the BODY forms.

See get-setf-method for a description of what a setf-method contains."
  (` (apply
      (function (lambda (, vars) (,@ body)))
      (get-setf-method (, place-form)))))

;;; This is much more complicated looking that it really is...
(defmacro setf-update-form (params &rest body)
  "(setf-update-form (OLD-VALUE-VAR PLACE-FORM [RETURN-VALUE]) BODY...)
evaluates the BODY forms to produce a form that will generate a new
value for the place.  The resulting generated forms will always be evaluated
*after* any arguments to the place-forms are evaluated.
OLD-VALUE-VAR should be either NIL, or a symbol, in which case it is
bound to a symbol that will be bound during the evaluation of the generated
form to the old value of PLACE-FORM.  RETURN-VALUE, if supplied, should
evaluate to a form that will form the return value.  The resulting form is
always evaluated *after* any other forms are (and the value of OLD-VALUE-VAR
is still bound).  By default, the new-value of the place is returned.

 [does this sound confusing?  It's really not...]

Examples:
  (defmacro ++ (place)
    (setf-update-form (old place)
       (` (+ (, old) 1))))
  (defmacro pop (place)
    (setf-update-form (old place (` (car (, old))))
      (` (cdr (, old)))))"
  (let ((old-value-form-var (car params))
	(place-form-form (car (cdr params)))
	(place-form-var (gensym))
	(update-fun-var (gensym))
	(ret-val-form (car (cdr (cdr params)))))
    (` (let* (((, place-form-var)
	       (%setfable-form-or-lose (, place-form-form)))
	      ((, update-fun-var)
	       (%get-setf-simple-update-fun (, place-form-var))))
	 (let* ((,@ (and old-value-form-var
			 (` ((old-value-var (gensym))
			     ((, old-value-form-var)
			      old-value-var))))))
	   (if (, (and (not ret-val-form)
		       (` (and (, update-fun-var)
			       (, (if old-value-form-var
				      (` (atom (, place-form-var)))
				      t))))))
	       ;; We can use a simple update-function (which avoids generating
	       ;; lots of temporary variables).
	       (append (list (, update-fun-var))
		       (if (atom (, place-form-var))
			   (list (, place-form-var))
			   (cdr (, place-form-var)))
		       (list
			(, (if old-value-form-var
			       (` (` (maybe-let (((, old-value-var)
						  (, (, place-form-var))))
				       (, (,@ body)))))
			       (letopt-maybe-progn body)))))
	       ;; We have to use the more general method...
	       (with-setf-method (temps val-forms store-vars
                                  store-form access-form)
		   (, place-form-var)
		 (` (maybe-let* ((,@ (%setf-zip temps val-forms))
				 (,@ (, (and old-value-form-var
					     (` (list
						 (list old-value-var
						       access-form))))))
				 ((, (car store-vars))
				  (, (, (letopt-maybe-progn body)))))
		       (, store-form)
		       (,@ (, (and ret-val-form
				   (` (list (, ret-val-form))))))))))
	   )))))

(defun get-setf-method (place-form)
  "Returns the `setf method' for the setf'able form PLACE-FORM.

A setf method is a list with the following elements:
 1.  A list of temp-variables to hold argument values.
 2.  A list of value-forms (subforms of PLACE-FORM) to whose values the
     temp-variables are to be bound.
 3.  A list of temporary variables, called store-variables, to hold the new values
     (this is always of length 1, as elisp doesn't have multiple values)
 4.  A `store-form,' which, assuming the above temporary variables are bound,
     will store the new value into the correct place, and return it.
 5.  An `access-form,' which, assuming the temp-variables are bound, will
     return the old value of PLACE-FORM.

If PLACE-FORM doesn't have a setf-method, an error will be signaled.

Setf-methods can be defined using defsetf or define-setf-method.

For all the gory details, see CLtL."
  (let ((place-form (%setfable-form-or-lose place-form)))
    (if (symbolp place-form)
	;; 
	(let ((store-var (gensym)))
	  (list nil nil (list store-var)
		(list 'setq place-form store-var) place-form))
	;;
	(let ((update-fun (%get-setf-simple-update-fun place-form))
	      (method (get (car place-form) 'setf-method)))
	  (cond (method
		 (apply method (cdr place-form)))
		(update-fun
		 (let ((store-var (gensym))
		       (temps (%setf-gensyms (1- (length place-form)))))
		   (list temps
			 (cdr place-form)
			 (list store-var)
			 (cons update-fun (append temps (list store-var)))
			 (cons (car place-form) temps)))))))))

;;; ----------------------------------------------------------------

(defmacro setf (place value &rest others)
  "(setf PLACE VAL PLACE VAL ...) stores each VAL into the corresponding PLACE.
Each PLACE is set before the next VAL is computed.

Each PLACE may be either a variable or a function call form that has an
associated setf-method.  Care is taken not to evaluate the sub-forms of any
PLACE more than once (note that only the sub-forms of each place, not the
places themselves, are evaluated).

See defsetf and define-setf-method for an explanation of how to add a
setf-method for a form that doesn't already have one.

Examples:
   (setf (car a) b)	; The same as (setcar a b)
   (setf (get a b) c)	; The same as (put a b c)"
  (cond (others
	 (` (progn (setf (, place) (, value))
		   (setf (,@ others)))))
	(t
	 (setf-update-form (nil place) value))))
			   
(defmacro psetf (&rest places-and-values)
  "(psetf PLACE VAL PLACE VAL ...) stores each VAL in parallel into the
corresponding PLACE; that is, all arguments (including the arguments to
each PLACE) are evaluated before any of the stores are done; all evaluation is
still done left-to-right.

Each PLACE may be either a variable or a function call form that has an
associated setf-method.  Care is taken not to evaluate the sub-forms of any
PLACE more than once (note that only the sub-forms of each place, not the
places themselves, are evaluated).

See defsetf and define-setf-method for an explanation of how to add a
setf-method for a form that doesn't already have one."
  (%psetf-expand places-and-values))

(defmacro shiftf (&rest places)
  "(shiftf PLACE1 ... PLACEN) stores the value of each PLACE<X+1> into PLACEX,
and returns the value of PLACE1.  The stores are done in `parallel'--
all arguments are evaluated before any stores are done; evaluation is
still done left-to-right.

Each PLACE may be either a variable or a function call form that has an
associated setf-method.  Care is taken not to evaluate the sub-forms of any
PLACE more than once.

See defsetf and define-setf-method for an explanation of how to add a
setf-method for a form that doesn't already have one.

Examples:
  ;; (car x) <- z, a <- the old value of (car x), return the old-value of a.
  (shiftf a (car x) z)
  ;;
  ;; Set the position of the marker X to nil, and return the old value.
  (shiftf (marker-position x) nil)"
  (%shiftf-expand places nil))

(defmacro rotatef (&rest places)
  "(rotatef PLACE1 ... PLACEN) stores the value of each PLACE<X+1> into PLACEX,
and stores the value of PLACE1 into PLACEN.  The return-value is NIL.
The stores are done in `parallel'-- all arguments are evaluated before any
stores are done; evaluation is still done left-to-right.

Each PLACE may be either a variable or a function call form that has an
associated setf-method.  Care is taken not to evaluate the sub-forms of any
PLACE more than once.

See defsetf and define-setf-method for an explanation of how to add a
setf-method for a form that doesn't already have one.

Examples:
  ;; Swap the car and cdr of x
  (rotatef (car x) (cdr x))"
  (%shiftf-expand places t))

(defun %psetf-expand (p-and-v &optional bindings stores)
  (if (null p-and-v)
      (cons 'maybe-let* (cons (nreverse bindings) (nreverse stores)))
      (let ((place (nth 0 p-and-v))
	    (value (nth 1 p-and-v)))
	(with-setf-method (temps val-forms store-vars store-form access-form)
	    place
	  (%psetf-expand (nthcdr 2 p-and-v)
			 (cons (list (car store-vars) value)
			       (nconc (%setf-zip temps val-forms)
				      bindings))
			 (cons store-form stores))))))

(defun %shiftf-expand (places wrap-p)
  (let ((first-var (gensym)))
    (%shiftf-expand-1 places first-var first-var nil nil wrap-p)))

(defun %shiftf-expand-1 (places prev-store-var first-var bindings stores wrap-p)
  (if (if wrap-p
	  (null places)
	  (null (cdr places)))		; save the last value
      (` (maybe-let* ((,@ (nreverse bindings))
		      ((, prev-store-var)
		       (, (if wrap-p first-var (car places)))))
	   (,@ (if wrap-p
		   (nreverse (cons nil stores))
		 (` ((prog1 (, first-var) (,@ (nreverse stores)))))))))
      (with-setf-method (temps val-forms store-vars store-form access-form)
	  (car places)
	(%shiftf-expand-1 (cdr places)
			  (car store-vars)
			  first-var
			  (cons (list prev-store-var access-form)
				(nconc (%setf-zip temps val-forms) bindings))
			  (cons store-form stores)
			  wrap-p))))

;;; ----------------------------------------------------------------

(defmacro define-setf-method (access-fun args &rest body)
  "(define-setf-method ACCESS-FUN ARGLIST BODY...) defines a `setf method'
for ACCESS-FUN.  The BODY forms are evaluated by setf with the ARGS bound to
the forms that are arguments to ACCESS-FUN in a setf place, and should return
the setf method.

For an explanation of what a setf-method contains, see get-setf-method.

It often suffices to use the simpler defsetf instead of define-setf-method.

For further details, see CLtL."
  (` (eval-and-compile
       (put '(, access-fun) 'setf-update-fun nil)
       (put '(, access-fun)
	    'setf-method
	    (function (lambda (, args) (,@ body))))
       '(, access-fun))))

(defmacro defsetf (access-fun &rest other-args)
  "Define a procedure for using setf with the function ACCESS-FUN.

Defsetf has two possible syntaxes:
 1. (defsetf ACCESS-FUN UPDATE-FUN) -- Means that UPDATE-FUN will store into
      the location accessed by ACCESS-FUN, and has the same arguments plus an
      additional argument for the new value (neither ACCESS-FUN or UPDATE-FUN
      is evaluted).
 2. (defsetf ACCESS-FUN ARGLIST (NEWVAL) BODY ...) -- This is sort of like
      defmacro (but including the extra (NEWVAL)).  When SETF needs to store
      into a location accessed by ACCESS-FUN, it will evaluate the BODY forms,
      which should return a form that does the update.  During the evaluation
      of the BODY forms, the variables in ARGLIST will be bound to forms which
      have the values of the corresponding arguments to ACCESS-FUN (but not
      necessarily the original forms; note that unlike with defmacro, you
      don't have to worry about protecting against multiple evaluation or
      evaluation order-- this is all taken care for you), and NEWVAL will be
      bound to some for which evaluates to the new value.

Examples:
 (defsetf aref aset)			; simple form
 (defsetf nth (index list) (val)	; complex form
    (` (setf (car (nthcdr (, index) (, list))) (, val))))

Even more complex setf behavior can be defined using define-setf-method.

For more details, see CLtL."
  (if (not (listp (car other-args)))
      ;; simple defsetf
      (` (eval-and-compile
	   (put '(, access-fun) 'setf-method nil)
	   (put '(, access-fun) 'setf-update-fun '(, (car other-args)))
	   '(, access-fun)))
      ;; complex defsetf
      (let* ((args (car other-args))
	     (store-var (car (car (cdr other-args))))
	     (body (cdr (cdr other-args))))
	(if (cdr (car (cdr other-args)))
	    (error "DEFSETF can only handle one store-variable"))
	(` (eval-and-compile
	     (put '(, access-fun) 'setf-update-fun nil)
	     (put '(, access-fun)
		  'setf-method
		  (function
		   (lambda (&rest place-args)
		     (let ((temps (%setf-gensyms (length place-args)))
			   (store-var (gensym)))
		       (list temps
			     place-args
			     (list store-var)
			     (let (((, store-var) store-var))
			       (apply (function
				       (lambda (, args)
					 (,@ body)))
				      temps))
			     (cons '(, access-fun) temps))))))
	     '(, access-fun))))))

;;; This isn't as useful as it is in common lisp, since elisp doesn't have
;;; default values for default arguments (so incf can't use it, e.g.)
(defmacro define-modify-macro (name args fun)
  (let ((place-var (gensym)))
    (` (defmacro (, name) (, (cons place-var args))
	 (setf-update-form (old-value (, place-var))
	   (` ((, '(, fun)) (, old-value) (, (,@ args)))))))))

(defmacro defsetf-and-return (access-fun update-fun)
  "Like the simple form of defsetf, but adds noise to return the new value.
Use this for functions who's update function takes all the arguments in the
proper order but don't return the right value.  See defsetf for more details."
  (` (defsetf (, access-fun) (&rest args) (value)
       (` (progn ((, '(, update-fun)) (,@ args) (, value))
		 (, value))))))

;;; ----------------------------------------------------------------
;;; some basic defsets

(defsetf aref aset)
(defsetf get put)
(defsetf car setcar)
(defsetf cdr setcdr)
(defsetf symbol-value set)
(defsetf symbol-function fset)
(defsetf symbol-plist setplist)

(defsetf nth (index list) (val)
  (` (setf (car (nthcdr (, index) (, list))) (, val))))
(defsetf nthcdr (index list) (val)
  (` (setf (cdr (nthcdr (1- (, index)) (, list))) (, val))))

(defsetf elt (sequence index) (val)
  (` (if (arrayp (, sequence))
	 (setf (aref (, sequence) (, index)) (, val))
	 (setf (nth (, index) (, sequence)) (, val)))))

;;; ----------------------------------------------------------------
;;; Handy macros.  The tests for common cases aren't really necessary, they
;;; just avoid some consing.

(defmacro push (value place)
  "Cons VALUE onto the front of the list in PLACE, and replace PLACE with the result.

PLACE may be either a variable or a function call form that has an associated
setf-method.  Care is taken not to evaluate the sub-forms of PLACE more than
once.

See defsetf and define-setf-method for an explanation of how to add a
setf-method for a form that doesn't already have one."
  (if (symbolp place)
      ;; common case
      (` (setq (, place) (cons (, value) (, place))))
      ;; general case
      (let ((val-var (gensym)))
	(` (maybe-let (((, val-var) (, value)))
	     ;; We bind the value first to preserve l-to-r arument evaluation.
	     ;; [This will be removed if possible by the let-optimizer]
	     (, (setf-update-form (old place)
		  (` (cons (, val-var) (, old))))))))))

(defmacro pop (place)
  "Replace PLACE with its cdr, and return its car.

PLACE may be either a variable or a function call form that has an associated
setf-method.  Care is taken not to evaluate the sub-forms of PLACE more than
once.

See defsetf and define-setf-method for an explanation of how to add a
setf-method for a form that doesn't already have one."
  (setf-update-form (old place (` (car (, old))))
    (` (cdr (, old)))))
    
(defmacro incf (place &optional amount)
  "Increment PLACE by AMOUNT (default 1).

PLACE may be either a variable or a function call form that has an associated
setf-method.  Care is taken not to evaluate the sub-forms of PLACE more than
once.

See defsetf and define-setf-method for an explanation of how to add a
setf-method for a form that doesn't already have one."
  (if (symbolp place)
      ;; common case
      (` (setq (, place) (+ (, place) (, (or amount 1)))))
      ;; general case
      (setf-update-form (old place)
	(` (+ (, old) (, (or amount 1)))))))

(defmacro decf (place &optional amount)
  "Decrement PLACE by AMOUNT (default 1).

PLACE may be either a variable or a function call form that has an associated
setf-method.  Care is taken not to evaluate the sub-forms of PLACE more than
once.

See defsetf and define-setf-method for an explanation of how to add a
setf-method for a form that doesn't already have one."
  (if (symbolp place)
      ;; common case
      (` (setq (, place) (- (, place) (, (or amount 1)))))
      ;; general case
      (setf-update-form (old place)
	(` (- (, old) (, (or amount 1)))))))

(defmacro swapf (place1 place2)
  "Exchange the values of PLACE1 and PLACE2.

PLACE1 and PLACE2 may be either variables or function call forms that have
associated setf-methods.  Care is taken not to evaluate the sub-forms of
either PLACE more than once.

The macro rotatef is exactly the same as swapf, but takes an arbitrary number
of arguments.

See defsetf and define-setf-method for an explanation of how to add a
setf-method for a form that doesn't already have one.

Examples:
  ;; Swap the car and cdr of x
  (swapf (car x) (cdr x))"
  (%shiftf-expand (list place1 place2) t))

;;; ----------------------------------------------------------------
;;; Emacs specific defsetfs.
;;; Note that we don't have to autoload these, since in order to use them, 
;;;   some other setf form must be called (which itself will be autoloaded).
;;;

;; random stuff
(defsetf default-value set-default)
(defsetf marker-position set-marker)
(defsetf get-register set-register)
(defsetf file-modes set-file-modes)
(defsetf-and-return match-data store-match-data)

;; buffer stuff
(defsetf current-buffer set-buffer)
(defsetf buffer-name (buffer) (value)
  (` (save-excursion
       (setf (current-buffer) (, buffer))
       (rename-buffer (, value))
       (, value))))
(defsetf buffer-modified-p set-buffer-modified-p)
(defsetf mark set-mark)
(defsetf point goto-char)
(defsetf syntax-table set-syntax-table)
(defsetf-and-return current-local-map use-local-map)
(defsetf-and-return current-global-map use-global-map)

;; window stuff
(defsetf window-buffer set-window-buffer)
(defsetf window-dot set-window-dot)
(defsetf window-hscroll set-window-hscroll)
(defsetf window-point set-window-point)
(defsetf window-start set-window-start)

;; screen stuff
(defsetf-and-return current-window-configuration
  set-window-configuration)
(defsetf screen-height set-screen-height)
(defsetf screen-width set-screen-width)

;; process stuff
(defsetf process-buffer set-process-buffer)
(defsetf process-filter set-process-filter)
(defsetf process-sentinel set-process-sentinel)
