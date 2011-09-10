;;; $Id: counter.el,v 1.4 1992/11/12 23:23:46 rwhitby Exp $ 
;;; $Copyright: (c) 1992  Rod Whitby, <rwhitby@research.canon.oz.au> $ 

;;; LCD Archive Entry:
;;; counter|Rod Whitby|rwhitby@research.canon.oz.au|
;;; Provides a counter for use in keyboard macros|
;;; 92/11/12|1.4|~/functions/counter.el.Z|

;;; Based on a news article by robert@cogsci.ed.ac.uk (Robert Inder)

;;; This file contains some simple functions for providing the
;;; user with access to a counter, for use in keyboard macros and
;;; the like.
;;;
;;; ESC := sets the counter to its argument, or 1
;;; ESC + increments the counter by its argument, or 1
;;; ESC * inserts counter value and increments by its argument, or 1

(provide 'counter)

(defvar counter-value 1 "*Counter value")

(make-variable-buffer-local 'counter-value)

(defun counter-set (initial) 
  "Set the counter to the argument value (default 1)"
  (interactive "p")
  (setq counter-value initial))

(defun counter-increment (inc)
   "Increments the counter by the argument value, if given, or 1"
   (interactive "p")
   (setq counter-value (+ counter-value inc)))

(defun counter-insert (increment)
  (interactive "*p")
  (insert (format "%d" counter-value))
  (counter-increment increment))

(define-key esc-map ":=" 'counter-set)
(define-key esc-map "+" 'counter-increment)
(define-key esc-map "*" 'counter-insert)
