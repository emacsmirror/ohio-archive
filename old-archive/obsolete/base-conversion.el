;;; base-conversion.el --- base conversion for integers.

;; Copyright (C) 1994 Simon Marshall.

;; Author: Simon Marshall <Simon.Marshall@mail.esrin.esa.it>
;; Keywords: numbers bases conversion
;; Version: 1.02

;; LCD Archive Entry:
;; base-conversion|Simon Marshall|Simon.Marshall@mail.esrin.esa.it|
;; Functions for integer convertion from base to base.|
;; 07-Apr-1994|1.02|~/misc/base-conversion.el.Z|

;;; This file is not part of GNU Emacs.

;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Purpose:
;; 
;; This package provides functions to convert integers to different bases.  All
;; functions return strings.
;;
;; Provided is the general interactive `number-to-number' function, and
;; specific `number-to-decimal' and `decimal-to-number' functions.  For
;; example:
;;
;; (number-to-number "127" 10 16)
;;      => "7f"
;; M-x number-to-number RET 10 RET 2 RET 16 RET
;;      => Convert 10 in base 2 to base 16: 2
;;
;; (number-to-decimal "1000101" 2)
;;      => "69"
;; (decimal-to-number 69 2)
;;      => "1000101"
;;
;; Also provided are conversion functions for ASCII characters specified in
;; hexadecimal, `hex-to-string' and `string-to-hex'.
;;
;; (hex-to-string "5f")
;;      => "_"
;; (string-to-hex " ")
;;      => "20"
;;
;; Lastly, and leastly, a general function to convert a number into a cons pair
;; list containing conversions into various bases.  Generally useless (even if
;; you are aware of `cdr' and `memq'), the function is `number-to-numbers'.
;;
;; (number-to-numbers "32" 10)
;;      => ((char . " ") (2 . "100000") (8 . "40") (10 . "32") (16 . "20"))
;; (number-to-numbers "32" 10 '(3 4 5 6 7))
;;      => ((3 . "1012") (4 . "200") (5 . "112") (6 . "52") (7 . "44"))

;; Installation:
;; 
;; To use, put in your ~/.emacs:
;;
;; (require 'base-conversion)
;;
;; or autoload the specific functions you require:
;;
;; (autoload 'number-to-number "base-conversion"
;;   "Return conversion of STRING in base BASE1 to string in base BASE2." t)

;; Feedback:
;;
;; This is hand written software.  Use it at your own risk.
;;
;; Please send me bug reports, bug fixes, and extensions, so that I can
;; merge them into the master source.
;;     - Simon Marshall (Simon.Marshall@mail.esrin.esa.it)
;; Don't forget the version number of the package.

;; History:
;;
;; - 1.00--1.01:
;;   Francesco Potorti` (pot@cnuce.cnr.it) for emacs-18 compatibility.
;; - 1.01--1.02:
;;   Made `number-to-number' interactive.
;;   Made `number-to-decimal' return a string like the others.
;;   Made `number-to-numbers' take a base list arg.
;;   Changed args string to number, decimal to number, and other doc fixes.
;;   Expanded Purpose section for instructions on use.
;;   Corrected dumb spelling mistakes in Installation section.

;;; User functions

(defun number-to-number (number base1 base2)
  "Return conversion of NUMBER in base BASE1 to string in base BASE2.
If called interactively, prompt and display in the minibuffer.
NUMBER may be a number or string.
See `number-to-decimal' and `decimal-to-number'."
  (interactive
   (let* ((number (read-from-minibuffer "Convert: "))
	  (base1 (read-from-minibuffer
		  (format "Convert %s in base: " number) nil nil t))
	  (base2 (read-from-minibuffer
		  (format "Convert %s in base %d to base: " number base1)
		  nil nil t)))
     (list number base1 base2)))
  (let ((value (decimal-to-number (number-to-decimal number base1) base2)))
    (if (interactive-p)
	(message "Convert %s in base %d to base %d: %s"
		 number base1 base2 value)
      value)))

(defun number-to-decimal (number base)
  "Return decimal conversion string of NUMBER in base BASE.
NUMBER may be a number or string.  Does range checking of NUMBER.
Deals with integers only."
  (let ((number (if (stringp number) number (number-to-string number))))
    (if (string-match "^-" number)
	(- (number-to-decimal (substring number 1) base))
      (let ((value 0) (index 0) (len (length number)))
	(while (< index len)
	  (setq value (+ (* value base) 
			 (digit-to-value (substring number index (1+ index))
					 base))
		index (1+ index)))
	(number-to-string value)))))

(defun decimal-to-number (number base)
  "Return conversion of decimal NUMBER to a string in base BASE.
NUMBER may be a number or string.  There is no range checking of NUMBER.
Deals with integers only."
  (let ((number (if (numberp number) number (string-to-number number))))
    (if (< number 0)
	(concat "-" (decimal-to-number (abs number) base))
      (let ((string "") (divisor 1) value)
	(while (/= number 0)
	  (setq value (% (/ number divisor) base)
		string (concat (value-to-digit value) string)
		number (- number (* value divisor))
		divisor (* divisor base)))
	string))))

;;; Other useful functions

(defun hex-to-string (hex)
  "Convert arg HEX ascii to a one-character string.
HEX may be a hexadecimal number or string.
See `number-to-decimal' and `char-to-string'."
  (char-to-string (string-to-number (number-to-decimal hex 16))))

(defun string-to-hex (string)
  "Convert arg STRING to a hexadecimal ascii.
See `decimal-to-number' and `string-to-char'."
  (decimal-to-number (string-to-char string) 16))

(defun number-to-numbers (number base &optional bases)
  "Return list of cons pairs of number NUMBER in base BASE.
The list comprises base-value cons pairs in the form (base . value).
Third arg BASES, if given, is a list of bases.  If not given, the default list
of character (symbol char), binary, octal, decimal, and hexadecimal is used.
NUMBER may be a number or string."
  (let ((bases (or bases '(char 2 8 10 16))))
    (mapcar (function (lambda (b)
	      (cons b (if (numberp b)
			  (number-to-number number base b)
			(char-to-string (string-to-number
					 (number-to-decimal number base)))))))
	    bases)))

;;; Work horse functions

(defun digit-to-value (digit base)
  "Return decimal value of string DIGIT in base BASE.
Alphabetic digits may be in upper or lower case.  Does range checking of DIGIT."
  (let* ((ascii (string-to-char digit))
	 (value (cond ((and (>= ascii ?0) (<= ascii ?9)) (- ascii ?0))
		      ((and (>= ascii ?a) (<= ascii ?z)) (+ 10 (- ascii ?a)))
		      ((and (>= ascii ?A) (<= ascii ?Z)) (+ 10 (- ascii ?A)))
		      (t (error (format "Unknown digit `%s'" digit))))))
    (if (< value base)
	value
      (error (format "Digit `%s' to big for base %d" digit base)))))

(defun value-to-digit (value)
  "Return string digit representing decimal VALUE.
Alphabetic digits are lower case.  There is no range checking of VALUE."
  (if (and (>= value 0) (<= value 9))
      (number-to-string value)
    (char-to-string (+ (- value 10) ?a))))

;;; Functions for emacs-18

(or (fboundp 'abs) (defun abs (x) (if (< x 0) (- x) x)))
(or (fboundp 'number-to-string) (defun number-to-string (x) (format "%d" x)))

(provide 'base-conversion)

;;; base-conversion.el ends here
