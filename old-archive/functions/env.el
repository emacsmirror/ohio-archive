;    I hadn't been expecting this post to be archived, so I didn't package it
; very well (for example, I didn't put a copyright notice on the message so
; that it could be distributed under the terms of the GPL).  I would
; appreciate it if you would replace the archive you made with the following
; instead.  Thanks.
; 
;;; env.el - getenv and putenv functions that work with process-environment
;;; Copyright (C) 1992 Noah S. Friedman
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, you can either send email to this
;;; program's author (see below) or write to:
;;;
;;;              The Free Software Foundation, Inc.
;;;              675 Massachusetts Avenue.
;;;              Cambridge, MA 02139, USA. 
;;;
;;; Please send bug reports, etc. to friedman@prep.ai.mit.edu

;;; LCD Archive Entry:
;;; env|Noah S. Friedman|friedman@prep.ai.mit.edu|
;;; getenv and putenv functions that work with process-environment.|
;;; 92-07-15|1.0|~/functions/env.el.Z|

;;; Don't compile emacs with -DMAINTAIN_ENVIRONMENT.  It isn't very
;;; flexible---you can't create buffer-local environments with it.  
;;; Versions of GNU Emacs prior to 18.58 had bugs and process-environment
;;; didn't work very well, so I would suggest updating your emacs if it's
;;; older than 18.58. 


;; save old definition
(and (subrp (symbol-function 'getenv))
     (fset 'getenv-subr (symbol-function 'getenv)))

(defun getenv (var)
  "Return the value of the environment variable VAR, or nil if none
exists.  First look in process-environment, then in the actual environment."
  (let ((env process-environment)
	(pattern (concat "^" (regexp-quote var) "="))
	found)
    (while env
      (and (string-match pattern (car env))
	   (setq found (car env)))
      (setq env (cdr env)))
    (if found
	(substring found (match-end 0))
      (getenv-subr var))))


(defun putenv (var &optional value)
  "Set the environment variable VAR to VALUE in process-environment.
VALUE is optional.  If unspecified or nil, the variable VAR is removed
>from process-environment (it cannot be removed from the real
environment, however)."
  ;; Remove previous definition of var from process-environment, if it exists.
  (let ((env process-environment)
	(pattern (concat "^" (regexp-quote var) "=")))
    (while (and env
		(not (if (string-match pattern (car env))
			 (setq process-environment
			       (delq (car env) process-environment)))))
      (setq env (cdr env))))
  ;; Add new value, if any. 
  (if value
      (setq process-environment 
	    (cons (concat var "=" value) process-environment)))
  value)

;; eof

