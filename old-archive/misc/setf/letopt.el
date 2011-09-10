;;; $Header: /home/user3/miles/src/elisp/RCS/letopt.el,v 1.5 1992/04/21 17:28:40 miles Exp $
;;; ----------------------------------------------------------------
;;; letopt.el -- Let optimization
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
;;; This file contains functions that remove unecessary bindings from a let.
;;; A binding is considered unecessary if all uses can (hopefully without loss
;;; of efficiency) be replaced with their definition, without any change in
;;; the result.  Bindings here are assumed to be only used within the lexical
;;; scope of the let, so these routines should only be called if you know
;;; this to be the case (e.g., by the macro which produces the let).
;;;
;;; The main entry point is LETOPT-MAYBE-OPTIMIZE-LET.  LETOPT-MAYBE-PROGN
;;; shouldn't really be here, but it is.
;;;
;;; The only special forms recognized are: QUOTE, FUNCTION, SETQ, LET and LET*,
;;; (COND is converted to IF), PROGN,
;;;
;;; Any unknown functions will be considered to have side effects unless the
;;; name of the function has a non-NIL 'SIDE-EFFECT-FREE property.  Jamie
;;; Zawinski's byte compiler asserts this property for many common functions.
;;;
;;; [Much of this stuff probably ought to be part of the compiler, but it
;;; isn't.  Oh well.]
;;;

(provide 'letopt)

;;; The proper environment to use while compiling.  It's nil otherwise,
;;; and so doesn't cause any problems.
(defvar byte-compile-macro-environment)

;;; ----------------------------------------------------------------

(defmacro maybe-let (bindings &rest body)
  "(maybe-let VARLIST BODY...) is like let, but doesn't guarantee that the
variables will be bound, just that the result will be as if they were
*either* lexically or dynamically bound to the given values.  So you can't
depend on them being visible by (non-lexically enclosed) functions called by
this function (although you can't depend on them NOT being visible either)."
  (%letopt-optimize bindings body 'let))

(defmacro maybe-let* (bindings &rest body)
  "(maybe-let* VARLIST BODY...) is like let*, but doesn't guarantee that the
variables will be bound, just that the result will be as if they were
*either* lexically or dynamically bound to the given values.  So you can't
depend on them being visible by (non-lexically enclosed) functions called by
this function (although you can't depend on them NOT being visible either)."
  (%letopt-optimize bindings body 'let*))

(defun function-side-effect-free-p (function)
  (and (symbolp function)
       (get function 'side-effect-free)))

(defun letopt-maybe-progn (forms)
  "Return FORMS with a "
  (cond ((null forms) nil)
	((null (cdr forms)) (car forms))
	(t (cons 'progn forms))))

(defun letopt-maybe-optimize-form (form)
  "If FORM is a let or let*, return it with unecessary bindings removed.
A binding is considered unecessary if all uses can (hopefully without loss
of efficiency) be replaced with their definition, without any change in
the result.  Bindings here are assumed to be only used within the lexical
scope of the let, so these routines should only be called if you know
this to be the case (e.g., by the macro which produces the let)."
  (let ((form (macroexpand form byte-compile-macro-environment)))
    (if (or (atom form) (not (memq (car form) '(let let*))))
	form
	(%letopt-optimize (nth 1 form) (nth 2 form) (car form)))))

;;; ----------------------------------------------------------------
				  
(defun %letopt-eq-adjoin (thing list)
  (if (memq thing list)
      list
      (cons thing list)))

(defun %letopt-eq-delete-one (thing list)
  (cond ((null list) nil)
	((eq (car list) thing) (cdr list))
	(t
	 (let ((prev list) (tail (cdr list)))
	   (while tail
	     (cond ((eq (car tail) thing)
		    (setcdr prev (cdr tail))
		    (setq tail nil))
		   (t
		    (setq prev tail tail (cdr tail))))))
	 list)))

;;; ----------------------------------------------------------------
;;; Stuff to expand all macros in a form

;;; Returns FORM with all macros expanded
(defun %letopt-expand-macros (form)
  (cond ((atom form) form)
	((eq (car form) 'cond)
	 (if (null (cdr form))
	     nil
	     (` (if (, (%letopt-expand-macros
			(car (car (cdr form)))))
		    (, (%letopt-expand-macros
			(letopt-maybe-progn (cdr (car (cdr form))))))
		    (, (%letopt-expand-macros
			(` (cond (,@ (cdr (cdr form)))))))))))
	((and (eq (car form) 'setq) (nthcdr 3 form))
	 ;; we split up multiple setqs to make things easier below
	 (` (progn (setq (, (nth 1 form))
			 (, (%letopt-expand-macros (nth 2 form))))
		   (, (%letopt-expand-macros
		       (` (setq (,@ (nthcdr 3 form)))))))))
	((memq (car form) '(quote function))
	 (list (car form)
	       (%letopt-maybe-expand-macros-in-lambda (nth 1 form))))
	((memq (car form) '(let let*))
	 (nconc (list (car form)
		      (%letopt-expand-macros-in-bindings (nth 1 form)))
		(mapcar (function %letopt-expand-macros)
			(nthcdr 2 form))))
	(t
	 (let ((expanded
		(macroexpand form byte-compile-macro-environment)))
	   (cond ((eq expanded form)
		  (cons (%letopt-maybe-expand-macros-in-lambda (car form))
			(mapcar (function %letopt-expand-macros)
				(cdr form))))
		 (t
		  (%letopt-expand-macros expanded)))))))

(defun %letopt-expand-macros-in-bindings (bindings)
  (mapcar (function
	   (lambda (binding)
	    (list (car binding)
		  (%letopt-expand-macros (nth 1 binding)))))
	  bindings))

(defun %letopt-maybe-expand-macros-in-lambda (form)
  (if (or (atom form)
	  (not (eq (car form) 'lambda)))
      form
      (cons 'lambda
	    (cons (nth 1 form)
		  (mapcar (function %letopt-expand-macros)
			  (nthcdr 2 form))))))

;;; ----------------------------------------------------------------

;; This expects macros to be already expanded, and only expects to
;; find the special forms QUOTE, FUNCTION, SETQ, LET and LET*.  [most other
;; special forms can be treated as if they were arguments for this purpose]
(defun %letopt-find-free-variables (form &optional bound free)
  (cond ((symbolp form)
	 (if (memq form bound)
	     free
	     (%letopt-eq-adjoin form free)))
	((atom form)
	 free)
	;; note: we just look at setq like a normal function
	((memq (car form) '(quote function))
	 free)
	((memq (car form) '(let let*))
	 (let ((bindings (car (cdr form)))
	       (new-bound bound))
	   (while bindings
	     (setq free
		   (%letopt-find-free-variables (car (cdr (car bindings)))
						(if (eq (car form) 'let*)
						    new-bound
						    bound)
						free))
	     (setq new-bound (cons (car (car bindings)) new-bound))
	     (setq bindings (cdr bindings)))
	   (%letopt-find-free-variables-in-forms (cdr (cdr form))
						 new-bound
						 free)))
	(t
	 (%letopt-find-free-variables-in-forms (cdr form) bound free))))

(defun %letopt-find-free-variables-in-forms (forms &optional bound free)
  (while forms
    (setq free (%letopt-find-free-variables (car forms) bound free))
    (setq forms (cdr forms)))
  free)

;;; ----------------------------------------------------------------
;;; Stuff to destructivel replace all occurances of one form in another

;; This expects macros to be already expanded, and only expects to
;; find the special forms QUOTE, FUNCTION, SETQ, LET and LET*
;;
;; Destructive.
(defun %letopt-replace-in-form (from to form)
  (cond ((equal form from) to)
	((atom form) form)
	((memq (car form) '(quote function))
	 (%letopt-maybe-replace-in-lambda from to (nth 1 form))
	 form)
	((memq (car form) '(let let*))
	 (%letopt-replace-in-let from to (nth 1 form) (nthcdr 2 form)
				 (eq (car form) 'let*))
	 form)
	(t
	 (%letopt-maybe-replace-in-lambda from to (car form))
	 (%letopt-replace-in-forms from to (cdr form))
	 form)))

(defun %letopt-maybe-replace-in-lambda (from to form)
  (if (and (listp form) (eq (car form) 'lambda))
      (%letopt-replace-in-forms from to (nthcdr 2 form)))
  form)

(defun %letopt-replace-in-let (from to bindings forms let*p)
  (let ((boundp nil))
    (while (and bindings (or (not boundp) (not let*p)))
      (setcar (cdr (car bindings))
	      (%letopt-replace-in-form from to (nth 1 (car bindings))))
      (if (equal (car (car bindings)) from)
	  (setq boundp t))
      (setq bindings (cdr bindings)))
    (if (not boundp)
	(%letopt-replace-in-forms from to forms)))
  forms)

(defun %letopt-replace-in-forms (from to forms)
  (let ((tail forms))
    (while tail
      (setcar tail (%letopt-replace-in-form from to (car tail)))
      (setq tail (cdr tail))))
  forms)

;;; ----------------------------------------------------------------

;;; Find out how many times VAR is used in FORM, and any damaging side
;;; effects.
;;;
;;; Params:
;;;   VAR -- the variable
;;;   FORM -- the form we're looking at right now
;;;   DEPS -- either a list of the variables VAR depends on, or
;;;	a symbol that VAR is a direct copy of.
;;;   Uses -- A list of (#USES . CHANGED-DEPENDENCIES) sofar (see below for
;;;   	the meaning of this).
;;;
;;; Returns one of:
;;;   NIL -- The variable is rebound somewhere in form, or used in a context
;;;	where some of its dependencies aren't valid.
;;;   A list of (#USES . CHANGED-DEPENDENCIES) --
;;;   	#USES is the # of times VAR is used
;;;	CHANGED-DEPENDENCIES is the subset of DEPS which may have a different
;;;     meaning than at the time when VAR was bound (any use of VAR when
;;;     CHANGED-DEPENDENCIES is non-NIL means that we have to keep VAR).
;;;
;;; This expects macros to be already expanded, and only expects to
;;; find the special forms QUOTE, FUNCTION, SETQ, LET and LET*
;;;
(defun %letopt-var-uses (var form deps uses)
  ;;(princ (format "\nVar: %s, Uses: %s, Form: %s" var uses form))
  (cond ((eq var form)
	 (cond ((cdr uses) nil)
	       (t
		(setcar uses (1+ (car uses)))
		uses)))
	((atom form) uses)
	((memq (car form) '(function quote))
	 ;; Handle lambda expressions used as a value.  The lambda is
	 ;; probably called outside of this function, but we could still
	 ;; replace occurances of VAR, since nothing we can replace it with
	 ;; would be any _less_ dynamic.
	 ;; However, since it may be evaluated at any time after this
	 ;; occurance, we can't trust that there isn't some destructive
	 ;; function lurking after we analyze the lambda that would trash
	 ;; something before we can use it.  _SO_ we treat any occurance of
	 ;; VAR inside the lambda as forcing it to be bound.
	 (let ((uniq-tag (list 'force-binding)))
	   (%letopt-remove-changed-dep uniq-tag
	    (%letopt-var-uses-in-maybe-lambda var (nth 1 form) deps
	     (%letopt-add-changed-dep uniq-tag uses)))))
	((eq (car form) 'setq)
	 (setq uses (%letopt-var-uses var (nth 2 form) deps uses))
	 (let ((sets (nth 1 form)))
	   (cond ((null uses))
		 ((eq sets var)
		  (setq uses nil))
		 ((if (atom deps)
		      (eq sets deps)
		      (memq sets deps))
		  (%letopt-add-changed-dep sets uses)))) 
	 uses)
	((memq (car form) '(let let*))
	 (%letopt-var-uses-in-let var (nth 1 form) (nthcdr 2 form)
				  deps uses (eq (car form) 'let*)))
	((memq (car form) '(setcar setcdr rplaca rplacd aset fset set put))
	 ;; things that we know trash only their first arg
	 (%letopt-handle-function-side-effects
	  nil (list (nth 1 form)) deps
	  (%letopt-var-uses-in-forms var (cdr form) deps uses)))
	((memq (car form) '(progn save-excursion if))
	 ;; special forms that we know don't have any side effects
	 (%letopt-var-uses-in-forms var (cdr form) deps uses))
	(t
	 ;; a normal function call
	 (%letopt-var-uses-in-maybe-lambda var (car form) deps
	  (%letopt-handle-function-side-effects
	   (function-side-effect-free-p (car form))
	   (cdr form)
	   deps
	   (%letopt-var-uses-in-forms var (cdr form) deps uses))))))

(defun %letopt-handle-function-side-effects (se-free-p args deps uses)
  (cond ((or se-free-p (null uses) (atom deps))
	 uses)
	(t
	 ;; We can't use this becuase it assumes too much (different
	 ;; variables can point too the same thing...):
	 ;;(while args
	 ;;  (let ((arg (car args)))
	 ;;    (if (and (symbolp arg) (memq arg deps))
	 ;;        (%letopt-add-changed-dep arg uses)))
	 ;;  (setq args (cdr args)))
	 ;; Instead, trash everything:
	 ;;(princ (format "\nTRASH: %s => EVERYTHING" args))
	 (%letopt-add-changed-dep '(EVERYTHING) uses)
	 uses)))

;;; Add DEP to the set of changed dependencies in USES.  DEP should either be
;;; a variable that is in the set of current dependencies, or some non-atom
;;; tag that indicates a condition (that would invalidate the calculation of
;;; the variable we're trying to replace).
(defun %letopt-add-changed-dep (dep uses)
  (setcdr uses (cons dep (cdr uses)))
  uses)
(defun %letopt-remove-changed-dep (dep uses)
  (if uses
      (setcdr uses (%letopt-eq-delete-one dep (cdr uses))))
  uses)

;;; If VAR is a dependency, add it to the set of changed dependencies in USES
(defun %letopt-maybe-bind-in-uses (var deps uses)
  (if (and (listp deps) (memq var deps))
      (%letopt-add-changed-dep var uses)
      nil))

(defun %letopt-unbind-in-uses (bindings uses)
  (and uses
       (while bindings
	 (%letopt-remove-changed-dep (car bindings) uses)
	 (setq bindings (cdr bindings))))
  uses)

(defun %letopt-var-uses-in-maybe-lambda (var form deps uses)
  (if (or (atom form) (not (eq (car form) 'lambda)))
      uses
      (let ((deps-bound nil)
	    (params (nth 1 form)))
	(while params
	  (if (%letopt-maybe-bind-in-uses (car params) deps uses)
	      (setq deps-bound (cons (car params) deps-bound)))
	  (setq params (cdr params)))
	(%letopt-unbind-in-uses deps-bound
	 (%letopt-var-uses-in-forms var (nthcdr 2 form) deps uses)))))

(defun %letopt-var-uses-in-let (var bindings forms deps uses let*p)
  (let ((boundp nil)
	(deps-bound nil))
    (while (and bindings uses (or (not boundp) (not let*p)))
      (let ((binding (car bindings)))
	(setq uses (%letopt-var-uses var (nth 1 binding) deps uses))
	(cond ((null uses))
	      ((eq (car binding) var)
	       ;; the variable we're looking at is shadowed, nothing can happen
	       (setq boundp t))
	      ((%letopt-maybe-bind-in-uses (car binding) deps uses)
	       ;; we bind one of the things it depends on
	       (setq deps-bound (cons (car binding) deps-bound)))))
      (setq bindings (cdr bindings)))
    (%letopt-unbind-in-uses deps-bound
     (if (and uses (not boundp))
	 ;; undo any dependencies we bound
	 (%letopt-var-uses-in-forms var forms deps uses)
	 uses))))

(defun %letopt-var-uses-in-forms (var forms deps uses)
  (while (and forms uses)
    (setq uses (%letopt-var-uses var (car forms) deps uses))
    (setq forms (cdr forms)))
  uses)

;;; ----------------------------------------------------------------

;;; Return T if FORM is something we can reasonably have multiple copies of
;;; without worrying about side-effects, efficiency or correctness.
(defun %letopt-can-duplicate-p (form)
  (or ;; variable
      (atom form)
      ;; quoted atom (we don't duplicate quoted lists, because then they
      ;; wouldn't be eq)
      (and (memq (car form) '(quote function))
	   (atom (nth 1 form)))))

(defun %letopt-optimize-let-bindings (bindings forms let*p)
  (and bindings
       (let* ((other-bindings
	       (%letopt-optimize-let-bindings (cdr bindings) forms let*p))
	      (binding (car bindings))
	      (var (nth 0 binding))
	      (val-form (%letopt-expand-macros (nth 1 binding)))
	      (deps
	       (if (symbolp val-form)
		   val-form
		   (%letopt-find-free-variables val-form)))
	      (var-uses
	       (%letopt-var-uses-in-let
		var other-bindings forms deps (list 0) t)))
	 ;;(princ (format "\nOLB: (%s <= %s <%s>) => %s" var val-form deps var-uses))
	 (cond ((or (null var-uses)
		    (and (> (car var-uses) 1)
			 (not (%letopt-can-duplicate-p val-form))))
		;; can't optimize it away
		(cons binding other-bindings))
	       (let*p
		(%letopt-replace-in-let var val-form other-bindings forms t)
		other-bindings)
	       (t
		(%letopt-replace-in-forms var val-form forms)
		other-bindings)))))

(defun %letopt-optimize (bindings forms let-fun)
  (let* ((new-forms
	  (mapcar (function %letopt-expand-macros) forms))
	 (new-bindings
	  (%letopt-optimize-let-bindings
	   (%letopt-expand-macros-in-bindings bindings)
	   new-forms
	   (eq let-fun 'let*))))
    (cond ((null new-bindings)
	   (letopt-maybe-progn new-forms))
	  (t
	   (cons let-fun (cons new-bindings new-forms))))))

;;; ----------------------------------------------------------------
