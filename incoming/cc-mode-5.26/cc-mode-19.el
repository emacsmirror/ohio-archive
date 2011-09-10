;;; cc-mode-19.el --- compatibility library for Emacs and XEmacs 19

;; Copyright (C) 1985,1987,1992-1999 Free Software Foundation, Inc.

;; Authors:    1998-1999 Barry A. Warsaw and Martin Stjernholm
;;             1997 Barry A. Warsaw
;; Maintainer: bug-cc-mode@gnu.org
;; Created:    03-Jul-1997
;; Version:    See cc-mode.el
;; Keywords:   c languages oop

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file is necessary in order to run CC Mode 5 under Emacs 19.34
;; and MULE based on Emacs 19.34.

;;; Code:

(provide 'cc-mode-19) ; Before loading cc-defs to avoid a file load loop.
(eval-when-compile
  (let ((load-path
	 (if (and (boundp 'byte-compile-current-file)
		  (stringp byte-compile-current-file))
	     (cons (file-name-directory byte-compile-current-file)
		   load-path)
	   load-path)))
    (load "cc-defs" nil t)))

(require 'advice)


;; Emacs 19.34 requires the POS argument to char-after.  Emacs 20
;; makes it optional, as it has long been in XEmacs.
(or (condition-case nil
	(progn (char-after) t)
      (error nil))
    (progn
      (ad-define-subr-args 'char-after '(pos))
      (defadvice char-after (before c-char-after-advice
				    (&optional pos)
				    activate)
	"POS is optional and defaults to the position of point."
	(if (not pos)
	    (setq pos (point))))))

(if (fboundp 'char-before)
    ;; (or (condition-case nil
    ;;         (progn (char-before) t)
    ;;       (error nil))
    ;;
    ;; This test is commented out since it confuses the byte code
    ;; optimizer (verified in Emacs 20.2 and XEmacs 20.4).  The effect
    ;; of this is that the advice below may be activated in those
    ;; versions, which is unnecessary but won't break anything.  It
    ;; only occurs when this file is explicitly loaded; in normal use
    ;; the test in cc-mode.el will skip it altogether.

    ;; MULE based on Emacs 19.34 has a char-before function, but
    ;; it requires a position.  It also has a second optional
    ;; argument that we must pass on.
    (progn
      (ad-define-subr-args 'char-before '(pos &optional byte-unit))
      (defadvice char-before (before c-char-before-advice
				     (&optional pos byte-unit)
				     activate)
	"POS is optional and defaults to the position of point."
	(if (not pos)
	    (setq pos (point)))))
  ;; Emacs 19.34 doesn't have a char-before function.  Here's it's
  ;; Emacs 20 definition.
  (defsubst char-before (&optional pos)
    (if (not pos)
	(setq pos (point)))
    (char-after (1- pos))))

;; Emacs 19.34 doesn't have a functionp function.  Here's it's Emacs
;; 20 definition.
(or (fboundp 'functionp)
    (defun functionp (object)
      "Non-nil if OBJECT is a type of object that can be called as a function."
      (or (subrp object) (byte-code-function-p object)
	  (eq (car-safe object) 'lambda)
	  (and (symbolp object) (fboundp object)))))

;; Emacs 19.34 doesn't have a when macro.  Here's it's Emacs 20
;; definition.
(or (fboundp 'when)
    (defmacro when (cond &rest body)
      "(when COND BODY...): if COND yields non-nil, do BODY, else return nil."
      (list 'if cond (cons 'progn body))))

;; Emacs 19.34 doesn't have an unless macro.  Here's it's Emacs 20
;; definition.
(or (fboundp 'unless)
    (defmacro unless (cond &rest body)
      "(unless COND BODY...): if COND yields nil, do BODY, else return nil."
      (cons 'if (cons cond (cons nil body)))))


;;; cc-mode-19.el ends here
