;;; cc-guess.el --- guess indentation values by scanning existing code

;; Copyright (C) 1985,1987,1992-1999 Free Software Foundation, Inc.

;; Author:     1994-1995 Barry A. Warsaw
;; Maintainer: Unmaintained
;; Created:    August 1994, split from cc-mode.el
;; Version:    See cc-mode.el
;; Keywords:   c languages oop

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; This file contains routines that help guess the cc-mode style in a
;; particular region of C, C++, or Objective-C code.  It is provided
;; for example and experimentation only.  It is not supported in
;; anyway.  Some folks have asked for a style guesser and the best way
;; to show my thoughts on the subject is with this sample code.  Feel
;; free to improve upon it in anyway you'd like.  Please send me the
;; results.  Note that style guessing is lossy!
;;
;; The way this is intended to be run is for you to mark a region of
;; code to guess the style of, then run the command, cc-guess-region.

;;; Code:

(eval-when-compile
  (let ((load-path
	 (if (and (boundp 'byte-compile-current-file)
		  (stringp byte-compile-current-file))
	     (cons (file-name-directory byte-compile-current-file)
		   load-path)
	   load-path)))
    (load "cc-defs" nil t)))
(require 'cc-engine)


(defvar cc-guessed-style nil
  "Currently guessed style.")

(defvar cc-guess-conversions
  '((c . c-lineup-C-comments)
    (inher-cont . c-lineup-multi-inher)
    (string . -1000)
    (comment-intro . c-lineup-comment)
    (arglist-cont-nonempty . c-lineup-arglist)
    (cpp-macro . -1000)))
  

(defun cc-guess-region (start end &optional reset)
  "Sets `c-offset-alist' indentation values based on region of code.
Every line of code in the region is examined and the indentation
values of the various syntactic symbols in `c-offset-alist' is
guessed.  The first such positively identified indentation is used, so
if an inconsistent style exists in the C code, the guessed indentation
may be incorrect.

Note that the larger the region to guess in, the slower the
guessing. Previous guesses can be concatenated together, unless the
optional RESET is provided.

See `cc-guess-write-style' to find out how to save the guessed style,
and `cc-guess-view-style' for viewing the guessed style."
  (interactive "r\nP")
  (if (consp reset)
      (setq cc-guessed-style nil))
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (let* ((syntax (c-guess-basic-syntax))
	     (relpos (cdr (car syntax)))
	     (symbol (car (car syntax)))
	     point-indent relpos-indent)
	;; TBD: for now I can't guess indentation when more than 1
	;; symbol is on the list, nor for symbols without relpos's
	(if (or (/= 1 (length syntax))
		(not (numberp relpos))
		;; also, don't try to reguess an already guessed
		;; symbol
		(assq symbol cc-guessed-style))
	    nil
	  (back-to-indentation)
	  (setq point-indent (current-column)
		relpos-indent (save-excursion
				(goto-char relpos)
				(current-column)))
	  ;; guessed indentation is the difference between point's and
	  ;; relpos's current-column indentation
	  (setq cc-guessed-style
		(cons (cons symbol (- point-indent relpos-indent))
		      cc-guessed-style))
	  ))
      (forward-line 1))))

;;; cc-guess.el ends here
