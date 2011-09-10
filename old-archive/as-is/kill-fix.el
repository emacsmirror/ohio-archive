;; Enhancement to kill-all-local-variables
;; Copyright (C) 1988, 1991 Free Software Foundation, Inc.

;; This file is not officially part of GNU Emacs, but is being donated
;; to the Free Software Foundation.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;; LCD Archive Entry:
;; kill-fix|Joe Wells and Inge Frick|inge@nada.kth.se|
;; Enhancement to kill-all-local-variables|
;; 02-Apr-1993||~/as-is/kill-fix.el.Z|

;; Created by: Joe Wells, jbw@bucsf
;; Created on: 1988?
;; Modified on: Thu Aug  8 10:01:15 1991
;; Filename: kill-fix.el
;; Purpose: make kill-all-local-variables skip certain variables
;;
;; Last modified by: Inge Frick, inge@nada.kth.se
;; Fri Apr  2 16:47:39 1993
;; Changed so that the 'killing-local-variable-function' if it exists will
;; only be called for those variables that where really killed and not for
;; those that where preserved by the 'preserved' property or its equivalent
;; in emacs19. The argument to the 'killing-local-variable-function' is now
;; an alist with only the variables really killed.
;;
;; Fri Mar 26 10:16:57 1993  Inge Frick
;; Changed the name of 'exit-function' to 'killing-local-variable-function'.
;; For backward compatibility, the old name 'exit-function' can still be used.
;; This was suggested by Joe Wells.
;;
;; Sun Jan  3 15:52:42 1993  Inge Frick
;; Added the 'killing-local-variable-function' property. One purpose of this
;; is to allow minor modes to exit gracefully when their local variables are
;; killed.  When kill-all-local-variables kills a local variable that has an
;; killing-local-variable-function property then that property must be a
;; function of one argument that will be called with the alist of all old
;; local variables as argument.

;; save the original subr function definition of kill-all-local-variables
(or (fboundp 'original-kill-all-local-variables)
    (fset 'original-kill-all-local-variables
	  (symbol-function 'kill-all-local-variables)))

(defun kill-all-local-variables ()
  "Eliminate all the buffer-local variable values of the current buffer.
This buffer will then see the default values of all variables.
NOTE: This function has been modified to ignore buffer-local variables
      whose 'preserved' property is non-nil. For buffer-local variables
      that are killed and whose 'killing-local-variable-function' property
      is non-nil, that function is called with an alist of the killed
      variables as argument."
  (let* ((oldvars (buffer-local-variables)) (ovars oldvars) var)
    (original-kill-all-local-variables)
    (while oldvars
      (if (eq (setq var (car (car oldvars))) 0) nil ; skip over 18.56 bug
	(if (or (get var 'preserved) (get var 'permanent-local))
	    (progn
	      (make-local-variable var) (set var (cdr (car oldvars))))))
      (setq oldvars (cdr oldvars)))
    (setq oldvars			; Get an alist of those variables that
	  (setq ovars (kill-fix-setdiff ovars ; where really killed.
					(cons (cons 0 nil) ; skip 18.56 bug
					      (buffer-local-variables)))))
    (while oldvars
      (if (setq var (or (get (setq var (car (car oldvars)))
			     'killing-local-variable-function)
			(get var 'exit-function))) ; backward compatibility
	  (funcall var ovars))
      (setq oldvars (cdr oldvars)))))

(defun kill-fix-setdiff (x y)
  (cond
   ((null x) nil)
   ((assq (car (car x)) y) (kill-fix-setdiff (cdr x) y))
   (t (cons (car x) (kill-fix-setdiff (cdr x) y)))))

(provide 'kill-fix)
