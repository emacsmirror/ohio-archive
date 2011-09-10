;;; -*- Mode:Emacs-Lisp -*-

;;; This file is obsolete; the Emacs 19 byte compiler does this and
;;; much more.  The v19 byte-compiler works in v18, and is available 
;;; from archive.cis.ohio-state.edu in the file
;;; pub/gnu/emacs/elisp-archive/packages/bytecomp.tar.Z.  It is
;;; also included with Lucid GNU Emacs, a divergent fork of FSF's v19,
;;; which is available from labrea.stanford.edu in /pub/gnu/lucid/.

;;; A function call is about the most expensive thing that one can do in
;;; emacs-lisp.  This file defines `defsubst', which, in effect, defines
;;; an inline function (a function that is open-coded at compile time).
;;; It does this by defining a macro which evaluates its arguments like
;;; a function.
;;;
;;; this file also modifies the byte-compiler to expand and compile 
;;; top-level calls to macros, because otherwise defsubst-ed routines
;;; would always run interpreted (lose lose).
;;;
;;; Created by Jamie Zawinski <jwz@lucid.com>, 14-feb-91.

(defmacro defsubst (name arglist &rest body)
  "Same syntax as DEFUN, except that a function call will not be generated
when invoking this function: it is fast and undebuggable.  (But, if the
function this defines is called from code that is not byte-compiled, it
will actually run much slower...)

Actually, this defines a macro, not a function, so you can't funcall it.
But that's the price you have to pay for speed..."
  (let ((restp nil))
    (list 'defmacro name arglist
      (cons 'cons
        (list ''let
          (list 'cons
	    (cons 'list
	      (delq nil (mapcar (function (lambda (x)
				(cond ((eq x '&optional) nil)
				      ((eq x '&rest)
				       (setq restp t)
				       nil)
				      (t (list 'list (list 'quote x)
					   (if restp
					       (list 'cons ''list x)
					       x))))))
				arglist)))
	    (list 'quote body)))))))


;;; Since defsubst is a macro which expands into a defmacro (which should then
;;; be evaluated, compiled, and added to the compile-time macro-environment)
;;; it is necessary for the byte compiler to macroexpand top-level forms at
;;; compile time (and then compile them if possible.)  It should have done
;;; this all along anyway...
;;;
;;; It also makes (progn (foo ...)) at top level equivalent to (foo ...) at
;;; top level.  This means that defmacros may be enclosed inside progns and
;;; still make it in to the compilation environment.

(require 'byte-compile "bytecomp")

(if (not (fboundp 'defsubst-original-byte-compile-file-form))
    (fset 'defsubst-original-byte-compile-file-form
	  (symbol-function 'byte-compile-file-form)))

(defun byte-compile-file-form (form)
  (setq form (macroexpand form byte-compile-macro-environment))
  (if (eq (car form) 'progn)
      (cons 'progn (mapcar 'byte-compile-file-form (cdr form)))
      (defsubst-original-byte-compile-file-form form)))


(provide 'defsubst)
