;;; radix.el --- radix (base) manipulation for integers.

;; Copyright (C) 1994 Simon Marshall.

;; Author: Simon Marshall <Simon.Marshall@mail.esrin.esa.it>
;; Keywords: numbers radix base
;; Version: 1.05

;; LCD Archive Entry:
;; radix|Simon Marshall|Simon.Marshall@mail.esrin.esa.it|
;; Functions for the manipulation of integers in any radix (or base).|
;; 28-Jun-1994|1.05|~/misc/radix.el.Z|

;; The archive is archive.cis.ohio-state.edu in /pub/gnu/emacs/elisp-archive.

;;; This file is not part of GNU Emacs.

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Purpose:
;; 
;; This package provides functions to manipulate (convert, compare, etc.)
;; integers of different bases.  Some functions are also available as commands.
;; Note that these commands use `minibuffer-history' so you can reuse input.
;;
;; Provided is the general interactive `number-to-number' conversion function,
;; and specific `number-to-decimal' and `decimal-to-number' conversion
;; functions.  For example:
;;
;; (number-to-number "127" 10 16)
;;      => "7f"
;; M-x number-to-number RET 10 RET 2 RET 16 RET
;;      -| Convert 10 in radix 2 to radix 16: 2
;;
;; (number-to-decimal "1000101" 2)
;;      => "69"
;; (decimal-to-number 69 2)
;;      => "1000101"
;;
;; Operators can be applied to numbers using the interactive `apply-operator'
;; function:
;;
;; (apply-operator '* 16 '("1000" . 2) '("2" . 10) '("10" . 16))
;;      => "100"
;; M-x apply-operator RET + RET 1 RET 2 RET 1 RET 2 RET 2
;;      -| Apply + on 1 in radix 2 and 1 in radix 2 to radix 2: 10
;;
;; (apply-operator '= nil '("f" . 16) '("15" . 10))
;;      => t
;; M-x apply-operator RET < RET 15 RET 10 RET f RET 16 RET
;;      -| Apply < on 15 in radix 10 and f in radix 16: nil
;;
;; Also provided are interactive conversion functions for ASCII characters
;; specified in hexadecimal, `hex-to-string' and `string-to-hex'.
;;
;; (hex-to-string "5f")
;;      => "_"
;; M-x hex-to-string RET 20 RET
;;      -| Hexadecimal 20 is string " "
;;
;; (string-to-hex " ")
;;      => "20"
;; M-x string-to-hex RET _ RET
;;      -| String "_" is hexadecimal 5f

;; Installation:
;; 
;; To use, put in your package that uses these functions:
;;
;; (require 'radix)
;;
;; or autoload in your ~/.emacs the specific functions you require:
;;
;; (autoload 'number-to-number "radix"
;;   "Convert NUMBER in radix RADIX1 to string in radix RADIX2." t)
;; (autoload 'hex-to-string "radix"
;;   "Convert arg HEX ascii to a one-character string." t)
;; (autoload 'string-to-hex "radix"
;;   "Convert arg STRING to hexadecimal ascii." t)
;; (autoload 'apply-operator "radix"
;;   "Apply OPERATOR, returning in radix RADIX, to NUMBERS." t)

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
;;   Francesco Potorti` (pot@cnuce.cnr.it) for Emacs-18 compatibility.
;; - 1.01--1.02:
;;   Made `number-to-number' interactive.
;;   Made `number-to-decimal' return a string like the others.
;;   Made `number-to-numbers' take a base list arg.
;;   Changed args string to number, decimal to number, and other doc fixes.
;;   Expanded Purpose section for instructions on use.
;;   Corrected dumb spelling mistakes in Installation section.
;; - 1.02--1.03:
;;   Made `digit-to-value' take a character as well as a string.
;;   Made `number-to-decimal' use `aref' rather than `substring'.
;;   Made `hex-to-string' and `string-to-hex' interactive.
;;   Added `string-to-number' for Emacs-18 compatibility.
;;   Removed `number-to-numbers' and felt better for it.
;;   Added `apply-operator'.
;;   Changed name from base-conversion to radix.
;; - 1.03--1.05:
;;   Cleaned comments.
;;   Corrected Copyleft.

;;; Conversion functions

(defun number-to-number (number radix1 radix2)
  "Convert NUMBER in radix RADIX1 to string in radix RADIX2.
NUMBER may be a number or string.
See `number-to-decimal' and `decimal-to-number'."
  (interactive
   "sConvert: \nnConvert %s in radix: \nnConvert %s in radix %s to radix: ")
  (let ((value (decimal-to-number (number-to-decimal number radix1) radix2)))
    (if (interactive-p)
	(message "Convert %s in radix %d to radix %d: %s"
		 number radix1 radix2 value)
      value)))

(defun number-to-decimal (number radix)
  "Return decimal conversion string of NUMBER in radix RADIX.
NUMBER may be a number or string.  Does range checking of NUMBER.
Deals with integers only."
  (let ((number (if (stringp number) number (number-to-string number))))
    (if (string-match "^-" number)
	(- (number-to-decimal (substring number 1) radix))
      (let ((value 0) (index 0) (len (length number)))
	(while (< index len)
	  (setq value (+ (* value radix) 
			 (digit-to-value (aref number index) radix))
		index (1+ index)))
	(number-to-string value)))))

(defun decimal-to-number (number radix)
  "Return conversion of decimal NUMBER to a string in radix RADIX.
NUMBER may be a number or string.  There is no range checking of NUMBER.
Deals with integers only."
  (let ((number (if (numberp number) number (string-to-number number))))
    (if (< number 0)
	(concat "-" (decimal-to-number (abs number) radix))
      (let ((string "") (divisor 1) value)
	(while (/= number 0)
	  (setq value (% (/ number divisor) radix)
		string (concat (value-to-digit value) string)
		number (- number (* value divisor))
		divisor (* divisor radix)))
	string))))

(defun hex-to-string (hex)
  "Convert arg HEX ascii to a one-character string.
HEX may be a hexadecimal number or string.
See `number-to-decimal' and `char-to-string'."
  (interactive "sHexadecimal: ")
  (let ((char (string-to-number (number-to-decimal hex 16))))
    (if (interactive-p)
	(message "Hexadecimal %s is string \"%c\"" hex char)
      (char-to-string char))))

(defun string-to-hex (string)
  "Convert arg STRING to hexadecimal ascii.
See `decimal-to-number' and `string-to-char'."
  (interactive "sString: ")
  (let ((hex (mapconcat '(lambda (c) (decimal-to-number c 16)) string "")))
    (if (interactive-p)
	(message "String \"%s\" is hexadecimal %s" string hex)
      hex)))

;;; Application function

(defun apply-operator (operator radix &rest numbers)
  "Apply OPERATOR, returning in radix RADIX, to NUMBERS.
Converts the answer to radix RADIX if non-nil and if the answer is a number.
OPERATOR is applied to the list of converted numbers, and can return anything.
NUMBERS is a list of cons pairs of the form (NUMBER . RADIX).  NUMBER may be a
number or string.  If called interactively, the command asks for two numbers.
See `number-to-decimal' and `decimal-to-number'."
  (interactive
   (let* ((op (read-minibuffer "Apply: "))
	  (n1 (read-from-minibuffer (format "Apply %s on: " op)))
	  (r1 (read-minibuffer (format "Apply %s on %s in radix: " op n1)))
	  (n2 (read-from-minibuffer
	       (format "Apply %s on %s in radix %d and: " op n1 r1)))
	  (r2 (read-minibuffer
	       (format
		"Apply %s on %s in radix %d and %s in radix: " op n1 r1 n2)))
	  (r (and (numberp
		   (funcall op (string-to-number (number-to-decimal n1 r1))
			    (string-to-number (number-to-decimal n2 r2))))
		  (read-minibuffer
		   (format
		    "Apply %s on %s in radix %d and %s in radix %d to radix: "
		    op n1 r1 n2 r2)))))
     (list op r (cons n1 r1) (cons n2 r2))))
  (let* ((val (apply operator
		     (mapcar '(lambda (n)
				(string-to-number
				 (number-to-decimal (car n) (cdr n))))
			     numbers)))
	 (raw (or (not (numberp val)) (not (numberp radix))))
	 (value (if raw val (decimal-to-number val radix))))
    (cond ((not (interactive-p))
	   value)
	  ((not raw)
	   (message
	    "Apply %s on %s in radix %d and %s in radix %d to radix %d: %s"
	    operator (car (nth 0 numbers)) (cdr (nth 0 numbers))
	    (car (nth 1 numbers)) (cdr (nth 1 numbers)) radix value))
	  (t
	   (message "Apply %s on %s in radix %d and %s in radix %d: %s"
		    operator (car (nth 0 numbers)) (cdr (nth 0 numbers))
		    (car (nth 1 numbers)) (cdr (nth 1 numbers)) value)))))

;;; Conversion work horse functions

(defun digit-to-value (digit radix)
  "Return decimal value of DIGIT in radix RADIX.
DIGIT may be a character or a string, and alphabetic digits may be in upper or
lower case.  Does range checking of DIGIT."
  (let* ((ascii (if (stringp digit) (string-to-char digit) digit))
	 (value (cond ((and (>= ascii ?0) (<= ascii ?9)) (- ascii ?0))
		      ((and (>= ascii ?a) (<= ascii ?z)) (+ 10 (- ascii ?a)))
		      ((and (>= ascii ?A) (<= ascii ?Z)) (+ 10 (- ascii ?A)))
		      (t (error "Unknown digit `%c'" ascii)))))
    (if (< value radix)
	value
      (error "Digit `%c' to big for radix %d" ascii radix))))

(defun value-to-digit (value)
  "Return string digit representing decimal VALUE.
Alphabetic digits are lower case.  There is no range checking of VALUE."
  (if (and (>= value 0) (<= value 9))
      (number-to-string value)
    (char-to-string (+ (- value 10) ?a))))

;;; Functions for Emacs-18

(or (fboundp 'abs) (defun abs (x) (if (< x 0) (- x) x)))
(or (fboundp 'number-to-string) (fset 'number-to-string 'int-to-string))
(or (fboundp 'string-to-number) (fset 'string-to-number 'string-to-int))

(provide 'radix)

;;; radix.el ends here
