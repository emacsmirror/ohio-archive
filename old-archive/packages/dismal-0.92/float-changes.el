;;;; -*- Mode: Emacs-Lisp -*- 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; File            : float-changes.el
;;;; Author          : Frank Ritter
;;;; Created On      : Fri Mar 20 16:56:37 1992
;;;; Last Modified By: Frank Ritter
;;;; Last Modified On: Mon Aug 17 23:36:58 1992
;;;; Update Count    : 12
;;;; 
;;;; PURPOSE
;;;; 	Changes to float.el to support integer and floats in the same 
;;;; calculations (with corercion to floats).
;;;; TABLE OF CONTENTS
;;;; 	i.	New Variables and constants
;;;;	I.	Change to f+ 
;;;;
;;;; 
;;;; Copyright 1991, Frank Ritter.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Status          : Unknown, Use with caution!
;;;; HISTORY
;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'float-changes)

(if (fboundp 'proclaim-inline)
  (proclaim-inline
    float-stringp
    integer-stringp
))


;;;
;;; 	i.	New Variables and constants
;;;

(defconst floating-point-leading-digits-regexp
   "^\\([0-9]*\\)\\(\\.[0-9]*\\|\\)$")

(defconst integer-regexp "^[ \t]*\\(-?\\)\\([0-9]+\\)[ \t]*$"
  "Regular expression to match integer numbers.  Exact matches:
1 - minus sign
2 - integer part
3 - end of string")

(defconst floating-point-list-regexp
  "^[ \t]*\\((quote\\)[ ]*\
\\(([0-9]+\\)[ ]+\\(\\.\\)[ ]+\
\\(-?\\)\
\\([0-9]+\\)\
))"
  "Regular expression to match floating point numbers as lists in strings.  
Extract matches:
0 - leading spaces
1 - quote
2 - more spaces
3 - ( and leading number
4 - dot, spaces
5 - optional negative sign
5 - more numbers
6 - two more closing parens
")

;(float-stringp "23.0000")
;(float-stringp ".0000")
;(float-stringp "   ")
;(float-stringp "-")
;(float-stringp "   .")
;(float-stringp "(quote (13 . -123123))")
(defun float-stringp (astring)
  (and (or (string-match floating-point-regexp astring 0)
           (string-match floating-point-list-regexp astring 0))
       (not (string-match "^[-. \t]*$" astring 0))))

(defun integer-stringp (astring) ;(integer-stringp "23")
  (string-match integer-regexp astring 0))

;; the - may need a \\, but doesn't look like it.  3-Aug-92 -FER
; (defconst floating-point-regexp
;   "^[ \t]*\\(-?\\)\\([0-9]*\\)\
; \\(\\.\\([0-9]*\\)\\|\\)\
; \\(\\(\\([Ee]\\)\\(-?\\)\\([0-9][0-9]*\\)\\)\\|\\)[ \t]*$"
;   "Regular expression to match floating point numbers.  Extract matches:
; 1 - minus sign
; 2 - integer part
; 4 - fractional part
; 8 - minus sign for power of ten
; 9 - power of ten
; ")


;;;
;;;	I.	Change to f+ 
;;; to work with negative numbers and crossed signs
;;;

(defun f+ (a1 a2)
  "Returns the sum of two floating point numbers."
  (let ((f1 (fmax a1 a2))
	(f2 (fmin a1 a2)))
     ;; always shift right to avoid overflow, not just pos numbers
     (setq f1 (fashr f1)
	   f2 (fashr f2))
    (normalize
     (cons (+ (car f1)
              (ash (car f2) (- (cdr f2) (cdr f1))))
	   (cdr f1)))))


;  EXPRESSION       b                       RESULT           DESIRED
; (f 1)                                    (4194304 . -23)  ok
; (float-to-string (f- (f 0) (f 1)))       -2.0             -1.0
; (float-to-string (f- (f 1)))             -1.0             ok
; (float-to-string (f- (f 0) (f- (f 1))))   1.              ok
; (float-to-string (f- (f 0) (f 1)))       -2.              -1.0
; (float-to-string (f- (f 100) (f 10)))    90.              ok
; (float-to-string (f- (f 100) (f 90)))    10.              ok
; (float-to-string (f- (f 100) (f 100)))    0               ok
; (float-to-string (f- (f 100) (f 110)))   -10.             ok
; (float-to-string (f- (f 100) (f 125)))   -25.             ok
; (float-to-string (f- (f 100) (f 175)))   -75.             ok
; (float-to-string (f- (f 100) (f 212)))   -112             ok
; (float-to-string (f- (f 100) (f 225)))   -125             ok
; (float-to-string (f- (f 100) (f 227)))   -127             ok
; (setq result (f- (f 100) (f 228)))
; (float-to-string (f- (f 100) (f 228)))
; (float-to-string (f- (f 100) (f 229)))   127             -129
; (float-to-string (f- (f 100) (f 231)))   125             -131
; (float-to-string (f- (f 100) (f 237)))   119             -137
; (float-to-string (f- (f 100) (f 250)))   106.            -125
; (float-to-string (f- (f 100) (f 500)))   112.            -400
; (float-to-string (f- (f 100) (f 1000)))  124.            -900

; (f- (f -128)) '(-4194304 . -15)
; (f+ (f 100) (f -228))  => (-8388608 . -16)
; (float-to-string (f+ (f 100) (f -228)))  => -128.00
; 100 = 6553600 . -16
; -228 = -228 . 0
; (setq f1 (f 100))
; (setq f2 (f -230))
; 1835008
; 
; ; (float-to-string (f+ (f 100) (f -228)))
; ; (f 100)  => '(6553600 . -16)
; ; (f -2000)
; ; (f* (aref powers-of-10 6) _f10)
; ; (aset powers-of-10 6 '(8000000 . -3))
; ; (float-to-string (fashr (f 1)))
; ; (float-to-string (f 1))
; 
; ; (float-to-string '(-8388608 . -16))
; ; (setq fnum '(-8388608 . -16))
; ; (setq value (fabs fnum))
; (cons (abs (car fnum)) (cdr fnum))
; 
; (abs -1)       ok
; (abs -4000000) ok
; (abs -6000000) ok
; (abs -8000000) ok
; (abs -8200000) ok
; (abs -8300000) ok
; (abs -8380000) ok
; (abs -8388000) ok
; (abs -8388600) ok
; (abs -8388607) ok
; (abs -8388608) bad
; (= -8388608 8388608)
; (+ (+ -8388608 -1) 1)
; 
; 
; (defconst decimal-digits 7)
; ;; support for decimal conversion routines
; (setq powers-of-10 (make-vector (1+ decimal-digits) _f1))
; (aset powers-of-10 1 _f10)
; (aset powers-of-10 2 '(6553600 . -16))
; (aset powers-of-10 3 '(8192000 . -13))
; (aset powers-of-10 4 '(5120000 . -9))
; (aset powers-of-10 5 '(6400000 . -6))
; (aset powers-of-10 6 '(8000000 . -3))
; (aset powers-of-10 7 (f* (f 10) (aref powers-of-10 6)))
; 
; (setq all-decimal-digs-minval (aref powers-of-10 (1- decimal-digits))
;       highest-power-of-10 (aref powers-of-10 decimal-digits))


;;;
;;;	II.	Add doc to ftrunc
;;;

;; incorporated in the float.el file, so only documented here.
;;    12-Jul-93 -FER
;; (defun ftrunc (fnum)			; truncate fractional part
;;  "Truncate the fractional part of a floating point number."
;;   (cond ((natnump (cdr fnum))		; it's all integer, return number as is
;; 	 fnum)
;; 	((<= (cdr fnum) (- maxbit))	; it's all fractional, return 0
;; 	 '(0 . 1))
;; 	(t				; otherwise mask out fractional bits
;; 	 (let ((mant (car fnum)) (exp (cdr fnum)))
;; 	   (normalize 
;; 	    (cons (if (natnump mant)	; if negative, use absolute value
;; 		      (ash (ash mant exp) (- exp))
;; 		    (- (ash (ash (- mant) exp) (- exp))))
;; 		  exp))))))
