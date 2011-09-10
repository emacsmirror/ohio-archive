;*****************************************************************************
;
; Filename:	maclib-toolbox.el
;
; Author:		as attributed below.
; Posted by:		Ken Wood, <kwood@austek.oz.au>
; Organisation:		Austek Microsystems Pty Ltd, Australia.
;
; Description:	Miscellaneous useful programming tools.
;
;*****************************************************************************

; $Id: maclib-toolbox.el,v 1.5 1991/10/23 02:16:57 kwood Exp $

(provide 'maclib-toolbox)


;********** Adding to hooks *********

; Author: Daniel LaLiberte, <liberte@cs.uiuc.edu>

(defun prepend-unique-hook (hook-var hook-function)
  "Prepend HOOK-VAR with HOOK-FUNCTION, if it is not already an element.
hook-var's value may be a single function or a list of functions."
  (if (boundp hook-var)
      (let ((value (symbol-value hook-var)))
	(if (and (listp value) (not (eq (car value) 'lambda)))
	    (and (not (memq hook-function value))
		 (set hook-var
		      (cons hook-function value)))
	  (and (not (eq hook-function value))
	       (set hook-var
		    (list hook-function value)))))
    (set hook-var (list hook-function))
    ))

(defun postpend-unique-hook (hook-var hook-function)
  "Postpend HOOK-VAR with HOOK-FUNCTION, if it is not already an element.
hook-var's value may be a single function or a list of functions."
  (if (boundp hook-var)
      (let ((value (symbol-value hook-var)))
	(if (and (listp value) (not (eq (car value) 'lambda)))
	    (and (not (memq hook-function value))
		 (set hook-var
		      (append value (list hook-function ))))
	  (and (not (eq hook-function value))
	       (set hook-var
		    (append value (list hook-function))))))
    (set hook-var (list hook-function))
    ))


; Author: Lynn Slater, <lrs@indetech.com>

(defun true-mode-name ()
  "Returns the name of the mode in such a form that the mode may be
  re-established by calling the function named by appending '-name' to
  this string.  This differs from the variable called mode-name in that
  this is guaranteed to work while the values held by the variable may
  have embedded spaces or other junk.

  THIS MODE NAME IS GUARANTEED OK TO USE IN THE MODE LINE."
  (let ((major-mode-name (symbol-name major-mode)))
    (substring major-mode-name 0
	       (or   (string-match "-mode" major-mode-name)
		     (length major-mode-name)))))
