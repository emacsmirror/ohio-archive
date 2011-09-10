;;;; -*- Mode: Emacs-Lisp -*- 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; File            : ritter-math.el
;;;; Author          : Frank Ritter
;;;; Created On      : Fri Dec  6 16:09:31 1991
;;;; Last Modified By: Frank Ritter
;;;; Last Modified On: Wed Jun  3 13:40:45 1992
;;;; Update Count    : 7
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
    log10plus
))


;;;
;;;	I.	log10
;;;

(defun log10 (x)
  (if (< x 0) (error "log10 error."))
  (if (> x 1)
      (log10plus x)
    (if (= x 1)
        0
    (log10minus x))))

(defun log10plus (x)
  (let ((dividend (/ x 10)))
  (if (not (= 0 dividend))
      (+ 1 (log10plus dividend))
     0)))


;;;
;;;	II.	signp
;;;

(defmacro signp (arg)
  (list 'if (list '> arg 0) 1 -1))
