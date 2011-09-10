;;;; -*- Mode: Emacs-Lisp -*- 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; File            : ritter-math.el
;;;; Author          : Frank Ritter
;;;; Created On      : Fri Dec  6 16:09:31 1991
;;;; Last Modified By: Frank Ritter
;;;; Last Modified On: Wed Jul 15 15:30:22 1992
;;;; Update Count    : 10
;;;; 
;;;; PURPOSE
;;;; 	Some simple math extensions to elisp.
;;;; TABLE OF CONTENTS
;;;;	I.	log10
;;;;	II.	signp
;;;; 
;;;; Copyright 1991, Carnegie Mellon University.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Status          : Unknown, Use with caution!
;;;; HISTORY
;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ritter-math)

(if (fboundp 'proclaim-inline)
  (proclaim-inline
    log10    
    ;; log10plus  ; recursive, so hard to do this to...
    log10minus
))


;;;
;;;	I.	log10
;;;

(defun log10plus (x)
  (let ((dividend (/ x 10)))
  (if (not (= 0 dividend))
      (+ 1 (log10plus dividend))
     0)))

(defun log10minus (x)
  (error "log10minus in ritter-math not defined."))

(defun log10 (x)
  (if (< x 0) (error "log10 error."))
  (if (> x 1)
      (log10plus x)
    (if (= x 1)
        0
    (log10minus x))))


;;;
;;;	II.	signp
;;;

(defmacro signp (arg)
  (list 'if (list '> arg 0) 1 -1))

