;;; fuzz.el --- Compensate for floating-point roundoff error

;; Copyright (C) 1998 Will Mengarini

;; Author: Will Mengarini <seldon@eskimo.com>
;; URL: <http://www.eskimo.com/~seldon>
;; Created: Su 15 Mar 98
;; Version: 0.20, Mo 04 May 98
;; Keywords: extensions, float, floating point, fuzz

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Try this with C-x C-e:
;;   (= (+ 0.1 0.2) 0.3)
;; Running on an Intel 486/50 with hardware floating point, GNU 19.34.1
;; returns `nil' because of a general problem with the imprecise
;; representation of floating point numbers.  This package implements
;; functions that compensate for that.  For a general discussion of the
;; issue, see the Info node elisp|Numbers|Comparison of Numbers.

;; To use this package, first you'll need to copy this file to a directory
;; that appears in your load-path.  `load-path' is the name of a variable
;; that contains a list of directories Emacs searches for files to load.
;; To prepend another directory to load-path, put a line like
;; (add-to-list 'load-path "c:/My_Directory") in your .emacs file.

;; Then, put
;;   (require 'fuzz)
;; in your .emacs file.  That will make these functions available:
;;   fuzzy= (x y)
;;   fuzzy<= (x y)
;;   fuzzy/= (x y)
;;   fuzzy< (x y)
;;   fuzzy>= (x y)
;;   fuzzy> (x y)
;;   fuzzyzerop (x)
;;   fuzzyplusp (x)
;;   fuzzyminusp (x)
;;   fuzzyintegerp (x)
;;   fuzzywholenump (x)
;; Each has a meaning analogous to the corresponding bare function, except
;; that numbers are considered equal if they're "close enough", which is
;; defined in terms of this package's global variable `fuzz'.

;; You might need a different `fuzz' for your machine.  To find out, try
;; M-x fuzz-selftest.  If you get an error, try increasing `fuzz'
;; with M-x set-variable.  When you find a value that works, put a line like
;;   (setq fuzz 1.0e-9)
;; in your .emacs file.

;; If you need a different fuzz for a particular application, you could
;; localize the variable `fuzz' in a `let' form, or make it buffer-local.

;; The definition of `fuzzy=' is optimized for speed rather than absolute
;; consistency; it's not, for example, strictly commutative, although the
;; difference is unlikely to matter in practice.  If you want a more rigorous
;; definition, this one, hacked from Info|elisp|Numbers|Comparison of Numbers
;;   (defun fuzzy= (x y)
;;     (or (and (fuzzyzerop x) (fuzzyzerop y))
;;         (<= (/ (abs (- x y))
;;                (max (abs x) (abs y)))
;;             fuzz)))
;; might better meet your needs.  Because every dyadic comparison function in
;; this package is defined in terms of `fuzzy=', you can simply overwrite the
;; definition of `fuzzy=' with the above definition or your own (by
;; evaluating the defun after evaluating "(require 'fuzz)"), and everything
;; else should just work.  (Try M-x fuzz-selftest again to be sure.  Note
;; that the unhacked version of `fuzzy=' in Info fails the
;; (assert (fuzzyintegerp fuzz)) test.)

;; In practice, I find fastfuzz.el, which see, useful more often.

;;; Code:

;;;###autoload
(defvar fuzz 1.0e-14
  "*Used to calculate whether floats are fuzzy=.")

;; For symbols like 'fuzzyplusp I considered 'fuzzy-plusp, but that uses a
;; different naming style from symbols like 'fuzzy>; it inserts an unexpected
;; hyphen.  That'd be likely to confuse developers' memories, so they'd be
;; trying to use symbols like 'fuzzy-> or, for that matter, 'fuzzyplusp.
;; Also, the builtin 'wholenump already uses a wordscrunched style.

;;;###autoload
(defsubst fuzzyzerop (x)
  "Return t if NUMBER is within `fuzz' of zero."
  (<= (abs x) fuzz))

;;;###autoload
(defsubst fuzzyplusp (x)
  "Return t if NUMBER is positive and not within `fuzz' of zero."
  (> x fuzz))

;;;###autoload
(defsubst fuzzyminusp (x)
  "Return t if NUMBER is negative and not within `fuzz' of zero."
  (< x (- fuzz)))

;;;###autoload
(defun fuzzy= (x y)
  "Return t if 2 args are nearly equal; defined using the variable `fuzz'."
  (or (= x y)
      (if (fuzzyzerop x)
          (fuzzyzerop y)
        (fuzzyzerop (1- (/ y x))))))

;;;###autoload
(defsubst fuzzy/= (x y)
  "Return t if 2 args aren't nearly equal; defined using the variable `fuzz'."
  (not (fuzzy= x y)))

;;;###autoload
(defsubst fuzzy> (x y)
  "Return t if first arg is greater than second, and they're not nearly equal."
  (and (> x y) (not (fuzzy= x y))))

;;;###autoload
(defsubst fuzzy<= (x y)
  "Return t if first arg is less than second, or they're nearly equal."
  (or (< x y) (fuzzy= x y)))

;;;###autoload
(defsubst fuzzy< (x y)
  "Return t if first arg is less than second, and they're not nearly equal."
  (and (< x y) (not (fuzzy= x y))))

;;;###autoload
(defsubst fuzzy>= (x y)
  "Return t if first arg is greater than second, or they're nearly equal."
  (or (> x y) (fuzzy= x y)))

;;;###autoload
(defsubst fuzzyintegerp (x)
  "Return t if ARG is close enough to an integer to be construed as one."
  (fuzzy= (fround x) x))

;;;###autoload
(defsubst fuzzywholenump (x)
  "Return t if ARG is close enough to a whole number to be construed as one."
  (and (fuzzyintegerp x) (or (plusp x) (fuzzyzerop x))))

;;; Selftest:
;; 
;; Use `delete-rectangle', orthodoxily bound to C-x r d, to uncomment this
;; code for automated regression testing.  It's commented out only to save
;; space (at RMS's request) in released Emacs; if you're going to hack it you
;; probably want to leave the selftest enabled.
;; 
;; (require 'cl)
;; 
;; (defun fuzz-selftest ()
;;   "Test the functions in fuzz.el: `fuzzy=', etc.
;; Signal an error if a test fails.
;; This selftest is not automatically run when fuzz.el is loaded,
;; so if you're on a new machine, or building a new Emacs,
;; you might want to run it by hand with \\[fuzz-selftest]."
;;   ;; I considered naming this `fuzzyselftest', but the name doesn't
;;   ;; really need to be symmetrical with floating point functions since
;;   ;; it's not a floating-point function; instead, I'd rather it be
;;   ;; symmetrical with a convention that selftesting packages contain a
;;   ;; NAME-selftest function where NAME is the same as the name of the
;;   ;; file.  That convention in particular is probably superior to one
;;   ;; using PREFIX-selftest where PREFIX is the namespace prefix that the
;;   ;; package reserves, because some packages reserve multiple prefixes.
;;   ;; The NAME-selftest convention would be much more convenient for use
;;   ;; by scripts that test groups of packages.
;;   (interactive)
;; 
;;   ;; This test suite is nothing like a comprehensive one; it just
;;   ;; tries to catch the most egregious brain farts.
;;   ;; Contributions would be welcomed.
;;   ;; With C-x C-e, this code
;;   ;;   (mapcar (lambda (x) (format "%.18g" x)) [.1 .2 .3])
;;   ;;   (mapcar (lambda (x) (format "%.18g" x)) [.4 .5 .6])
;;   ;;   (mapcar (lambda (x) (format "%.18g" x)) [.7 .8 .9])
;;   ;; may be useful in getting an idea how to construct problem expressions.
;; 
;;   (assert      (fuzzyplusp    0.01))
;;   (assert      (fuzzyminusp  -0.01))
;; 
;;   (assert (not (fuzzyzerop    0.01)))
;;   (assert (not (fuzzyzerop   -0.01)))
;; 
;;   (assert      (fuzzy= (* (/ 10.0 3.0) 3.0) 10.0))
;;   (assert      (fuzzy= (* (/ 01.0 3.0) 3.0) 01.0))
;; 
;;   (assert      (fuzzy=  (+ 0.1 0.2) 0.3))
;;   (assert      (fuzzy<= (+ 0.1 0.2) 0.3))
;;   (assert      (fuzzy>= (+ 0.1 0.2) 0.3))
;;   (assert (not (fuzzy/= (+ 0.1 0.2) 0.3)))
;;   (assert (not (fuzzy>  (+ 0.1 0.2) 0.3)))
;;   (assert (not (fuzzy<  (+ 0.1 0.2) 0.3)))
;; 
;;   (assert      (fuzzy= 1.1 (/ (/ (* 4.0 1.1) 2.0) 2.0)))
;; 
;;   (assert      (fuzzy<= 0.0 0.0))
;;   (assert      (fuzzy<= 0.0 0.1))
;; 
;;   (assert (not (fuzzy>  0.0 0.0)))
;;   (assert (not (fuzzy>  0.0 0.1)))
;; 
;;   (assert      (fuzzy>= 1.0 1.0))
;;   (assert      (fuzzy>= 1.1 1.0))
;; 
;;   (assert (not (fuzzy<  1.0 1.0)))
;;   (assert (not (fuzzy<  1.1 1.0)))
;; 
;;   (assert      (fuzzy<  +2.0 +3.0))
;;   (assert      (fuzzy<= +2.0 +3.0))
;;   (assert (not (fuzzy>  +2.0 +3.0)))
;;   (assert (not (fuzzy>= +2.0 +3.0)))
;; 
;;   (assert (not (fuzzy<  +2.0 -3.0)))
;;   (assert (not (fuzzy<= +2.0 -3.0)))
;;   (assert      (fuzzy>  +2.0 -3.0))
;;   (assert      (fuzzy>= +2.0 -3.0))
;; 
;;   (assert      (fuzzy<  -2.0 +3.0))
;;   (assert      (fuzzy<= -2.0 +3.0))
;;   (assert (not (fuzzy>  -2.0 +3.0)))
;;   (assert (not (fuzzy>= -2.0 +3.0)))
;; 
;;   (assert (not (fuzzy<  -2.0 -3.0)))
;;   (assert (not (fuzzy<= -2.0 -3.0)))
;;   (assert      (fuzzy>  -2.0 -3.0))
;;   (assert      (fuzzy>= -2.0 -3.0))
;; 
;;   (assert      (fuzzyzerop      fuzz))
;;   (assert      (fuzzyintegerp   fuzz))
;;   (assert      (fuzzywholenump  fuzz))
;; 
;;   (assert      (fuzzyzerop      0.0))
;;   (assert      (fuzzyintegerp   0.0))
;;   (assert      (fuzzywholenump  0.0))
;; 
;;   (assert      (fuzzyintegerp   1.0))
;;   (assert      (fuzzywholenump  1.0))
;; 
;;   (assert      (fuzzyintegerp  -1.0))
;; 
;;   (message "Fuzz selftest successful")
;; 
;;   ) ;for C-x C-e: (fuzz-selftest)
;;
;;; End of selftest

(provide 'fuzz)

;;; fuzz.el ends here