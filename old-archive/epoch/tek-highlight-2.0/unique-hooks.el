;;*****************************************************************************
;;
;; Filename:	unique-hooks.el
;;
;; Copyright (C) 1992  Rod Whitby
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; Modified by:		Rod Whitby, <rwhitby@research.canon.oz.au>
;; Author:		Daniel LaLiberte, <liberte@cs.uiuc.edu>
;;
;; Description:	Prepend and postpend hook functions to hook variables if they
;;		are not already there.
;;
;;*****************************************************************************

;;; $Id: unique-hooks.el,v 1.2 1992/08/18 04:16:24 rwhitby Rel $ 

(defun prepend-unique-hook (hook-var hook-function)
  "Prepend HOOK-VAR with HOOK-FUNCTION, if it is not already an element.
HOOK-VAR's value may be a single function or a list of functions."
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
HOOK-VAR's value may be a single function or a list of functions."
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

(provide 'unique-hooks)
