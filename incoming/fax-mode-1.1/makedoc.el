;;; makedoc.el -- write documentation strings to the file `./DOC'.
;;;
;;; Copyright (C) 1995 Ralph Schleicher
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2 of
;;; the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


(load-file "./symbols")

(setq var-list ()
      func-list ())

(with-output-to-temp-buffer "*Help*"
  (mapcar (function
	   (lambda (symbol)
	     (and (boundp symbol) (user-variable-p symbol)
		  (not (memq symbol var-list))
		  (progn
		    (princ (format "Variable: %s\n%s\n\n\n" symbol
				   (or (documentation-property
					symbol 'variable-documentation)
				       "Not documented.")))
		    (setq var-list (cons symbol var-list))))
	     (and (fboundp symbol) (commandp symbol)
		  (not (memq symbol func-list))
		  (progn
		    (princ (format "Function: %s\n%s\n\n\n" symbol
				   (or (documentation symbol)
				       "Not documented.")))
		    (setq func-list (cons symbol func-list))))))
	  symbols))

(set-buffer "*Help*")

(untabify (point-min) (point-max))
(write-region (point-min) (point-max) "./DOC")
