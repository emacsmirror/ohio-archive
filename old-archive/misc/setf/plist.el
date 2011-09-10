;;; $Header: /home/user3/miles/src/elisp/RCS/plist.el,v 1.7 1992/04/21 17:32:15 miles Exp $
;;; ----------------------------------------------------------------
;;; plist.el -- Anonymous property lists.
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
;;; This file implements "anonymous" property-list functions, which are just
;;; property lists not bound to any symbol.
;;;
;;; In general, where the property list is stored in a place P (which can be
;;; a variable or any other form that can be set with setf), you look for
;;; properties using (getf P NAME), and change them using (setf (getf P NAME)
;;; VALUE).  So (getf (symbol-plist SYMBOL-NAME) NAME) is just like (get
;;; SYMBOL-NAME NAME).
;;;
;;; Getf also has the notion of a default value, which is what it returns
;;; when there isn't any property of the given name (as distuinguished from a
;;; property with a NIL value).
;;;
;;; So to increment the property 'zot in a property list stored in the car of
;;; the variable boing by 47, you do:
;;;   (incf (getf (car boing) 'zot 0) 47)
;;; Note the use of the default value zero to avoid an error if the property
;;; doesn't exist yet.
;;;

(provide 'plist)

(require 'setf)
(require 'gensym)

(defun getf (plist propname &optional default)
  "Return the value from the property list PLIST of the property PROPNAME.
If there is no such property, return DEFAULT instead (which is optional and
defaults to NIL).

May be used as a place with setf to change a property."
  (while (and plist (not (eq (car plist) propname)))
    (setf plist (cdr (cdr plist))))
  (if plist
      (car (cdr plist))
      default))

(defun %putf (plist propname val)
  "Don't use this, use (setf (getf PLIST PROPNAME) VALUE) instead."
  (let ((tail plist))
    (while (and tail (not (eq (car tail) propname)))
      (setf tail (cdr (cdr tail))))
    (cond (tail
	   (setf (car (cdr tail)) val)
	   plist)
	  (t
	   (cons propname (cons val plist))))))

(defun %plist-tail (plist propname)
  (while (and plist (not (eq (car plist) propname)))
    (setf plist (cdr (cdr plist))))
  plist)

(define-setf-method getf (place propname &optional default)
  (with-setf-method (temps values stores store-form access-form)
      place
    (let ((val-var (gensym))
	  (propname-var (gensym))
	  (def-var (if default (gensym))))
      (list (append temps (list propname-var) (and default (list def-var)))
	    (append values (list propname) (and default (list default)))
	    (list val-var)
	    (` (maybe-let (((, (car stores))
			    (%putf (, access-form)
				   (, propname-var)
				   (, val-var))))
		 (, store-form)
		 (, val-var)))
	    (` (getf (, access-form) (, propname-var)
		     (,@ (and default (list def-var)))))))))

(defmacro remf (place propname)
  "Remove from the anonymous property-list stored in PLACE the property PROPNAME.
Returns T if something was removed, NIL if nothing was done.

PLACE may be either a variable or a function call form that has an associated
setf-method.  Care is taken not to evaluate the sub-forms of PLACE more than
once.

See defsetf and define-setf-method for an explanation of how to add a
setf-method for a form that doesn't already have one."
  (with-setf-method (temps vals store-vars store-form access-form)
	place
    (let ((prop-var (gensym))
	  (head-var (gensym))
	  (tail-var (gensym))
	  (next-tail-var (gensym)))
      (` (maybe-let ((,@ (%setf-zip temps vals))
		     ((, prop-var) (, propname)))
	   (maybe-let* (((, head-var) (, access-form))
			((, tail-var) (%plist-tail (, head-var) (, prop-var))))
	     (and (, tail-var)
		  (let (((, next-tail-var) (nthcdr 2 (, tail-var))))
		    (if (eq (, head-var) (, tail-var))
			(maybe-let (((, (car store-vars)) (, next-tail-var)))
			   (, store-form))
			(psetf (car (, tail-var)) (car (, next-tail-var))
			       (cdr (, tail-var)) (cdr (, next-tail-var))))
		    t))))))))
