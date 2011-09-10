;;; fuse-math.el --- using calc with Atoms mode

;; Author:  Bruce Ravel <ravel@phys.washington.edu>
;; Maintainer:  Bruce Ravel <ravel@phys.washington.edu>
;; CReated: 10 May 1998
;; Version:  0.5.10
;; Keywords:  feff, uwxafs, input files, atoms, calc

;; $Id: Atoms-math.el,v 1.2 1998/03/14 22:46:00 bruce Exp $

;; Copyright (C) 1998, 1998 Bruce Ravel

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Massachusettes Ave,
;; Cambridge, MA 02139, USA.

;; Everyone is granted permission to copy, modify and redistribute this
;; and related files provided:
;;   1. All copies contain this copyright notice.
;;   2. All modified copies shall carry a prominant notice stating who
;;      made modifications and the date of such modifications.
;;   3. The name of the modified file be changed.
;;   4. No charge is made for this software or works derived from it.
;;      This clause shall not be construed as constraining other software
;;      distributed on the same medium as this software, nor is a
;;      distribution fee considered a charge.

;;; Commentary:

;; It may be desirable to run `Atoms-evaluate-buffer' whenever saving
;; the file.  This can be done by putting the following in the .fuse
;; file:
;;    (add-hook 'local-write-file-hooks 'Atoms-evaluate-buffer)
;; Another useful hook binding is
;;    (add-hook 'input-before-run-hook
;;	        '(lambda () (and (string= "atoms" input-program-name)
;;		                 (Atoms-evaluate-buffer)
;;                               (save-buffer))))
;; That one evaluates and saves the buffer prior to running Atoms.
;;
;; calc can be found at ftp://prep.ai.mit.edu/pub/gnu/calc-2.02f.tar.gz

;;; Code:

(require 'cl)
(require 'input)
(eval-when-compile (defvar Atoms-mode-map ()))

(defcustom Atoms-evaluation-comment-string "!+"
  "*Comment string identifying an evaluation line in Atoms-mode.
Note that this must begin with a comment character so that the
evaluation line will be ignored when Atoms runs.  See the document
string for `Atoms-evaluate-line' for more details.

Note that the string \"!+\" is currently hard-wired into the font-lock
regular expressions."
  :group 'fuse-programs
  :type 'string)
(defcustom Atoms-definition-comment-string "!-"
  "*Comment string identifying a definition line in Atoms-mode.
Note that this must begin with a comment character so that the
definition line will be ignored when Atoms runs.  See the document
string for `Atoms-evaluate-line' for more details.

Note that the string \"!-\" is currently hard-wired into the font-lock
regular expressions."
  :group 'fuse-programs
  :type 'string)

(defvar Atoms-variables-alist ()
  "Alist of evaluation variables and their values.
It is pointless to alter this by hand since `Atoms-read-definition'
overwrites it every time it is called.  Similarly, it is not useful to
alter calc's internal variables by hand, since those are
`makunbound'-ed by `Atoms-read-definition'")

;; definition functions

(defun Atoms-read-definition-line ()
  "Append or update all variables defined on the current line."
  (save-excursion
    (let ((eol (save-excursion (end-of-line) (point)))
	  word value (continue t))
      (when (input-definition-p)
	(back-to-indentation)
	(forward-char 2)
	(while continue
	  (forward-word 1)
	  (setq word (input-this-word))
	  (forward-word 1)
	  (if (> (point) eol)  ;; don't go over the end of the line!
	      (setq continue nil)
	    (setq value (input-this-word))
	    (if (assoc word Atoms-variables-alist)
		(nsubstitute (cons word value) ;; update
			     (assoc word Atoms-variables-alist)
			     Atoms-variables-alist)
	      (setq Atoms-variables-alist      ;; or append
		    (append Atoms-variables-alist
			    (list (cons word value))))) ))) )))

(defun Atoms-read-definitions ()
  "Make or update `Atoms-variables-alist'.
This finds and stores all variables defined in this buffer.  This
first unbinds all of the `calc' variables set by the previous
evaluation and resets `Atoms-variables-alist' to an empty list."
  (let ((expr (concat "^[ \t]*" (regexp-quote Atoms-definition-comment-string))))
    (save-excursion
      (dotimes (n (length Atoms-variables-alist) t)
	       (makunbound
		(intern (concat "var-" (car (elt Atoms-variables-alist n))))))
      (setq Atoms-variables-alist ())
      (goto-char (point-min))
      (while (re-search-forward expr (point-max) t)
	(Atoms-read-definition-line)) ) ))

;; evaluation functions

(defun Atoms-evaluate-line (&optional arg)
  "Evaluate the math expressions on the current line.
If ARG is non-nil, then the definition lines will not be re-scanned.

This function is an interface to the calc package, which is used to
evaluate the math expressions.  Those familiar with `calc' can also
use `calc' in embedded mode.  This function and
`Atoms-evaluate-buffer' are convenient for local mode-map bindings and
for hooks.

There are certain syntax rules that must be followed, some required by
`calc' and some by FUSE.

Variable definitions are made on comment lines beginning with
'Atoms-definition-comment-string' (\"!-\" by default) and are
variable/value pairs.  See the example below.

Evaluation lines begin with `Atoms-evaluation-comment-string' (\"!+\"
by default).  This function does not require evaluation lines to be in
the atoms list, but they will confuse the program Atoms if they appear
before the atoms list.

Here is an example of an atoms input file marked up to use this
function:

   title PbTiO3 N&K,10K,a=3.885,c=4.139
   Space  P 4 m m
   a=3.885   c=4.139   rmax=7   core=ti

   !- ti = 0.0377	o1 = 0.1118	o2 = 0.1174
   !- q = 1

   atom
   ! At.type  x        y       z      tag
      Pb    0.0     0.0     0.0
   !+ Ti    0.5	    0.5	 (0.5 + q*ti)
   !+ O	    0.5	    0.5	 (0.0 + q*o1)	axial
   !+ O	    0.0	    0.5	 (0.5 + q*o2)	planar

Four variables are defined, three of which represent the tetragonal
distortions in this crystal.  The fourth is a scaling factor for these
distortions.  After evaluating the first line beginning with \"!+ O\"
the input file will look like this:

   title PbTiO3 N&K,10K,a=3.885,c=4.139
   Space  P 4 m m
   a=3.885   c=4.139   rmax=7   core=ti

   !- ti = 0.0377	o1 = 0.1118	o2 = 0.1174
   !- q = 1

   atom
   ! At.type  x        y       z      tag
      Pb    0.0     0.0     0.0
   !+ Ti    0.5	    0.5	 (0.5 + q*ti)
   !+ O	    0.5	    0.5	 (0.0 + q*o1)	axial
      O	    0.5	    0.5	 0.1118         axial
   !+ O	    0.0	    0.5	 (0.5 + q*o2)	planar

Note that the element symbol and the tag are preserved when the
evaluated line is written out.  When the line is evaluated, the tag is
compared with the tag on the following line.  If it matches, then the
following line is deleted and the evaluated line is inserted.  If the
tag does not match, then it is assumed that the following line is for
a different unique position and the evaluated line is inserted after
the evaluation line.

This function can be become confused about whether to insert or
replace an expanded line.  The surest way to avoid this confusion is
to use a unique tag with every atom position.

Also note that the definition and evaluation lines are valid comment
lines for the program Atoms and so will be ignored when Atoms runs.

Syntax rules for evaluation and definition lines:

1.  Only lines beginning with optional whitespace followed by either
    \"!-\" or \"!+\" will be interpreted as definition or evaluation
    lines, respectively.

2.  Definition lines may not themselves be math expressions.  They
    must be variable/value pairs.  They can, though, be fractions made
    of numbers without whitespace.  If the value is not an integer, a
    float, or a fraction of numbers, it will most likely evaluate to 0.

3.  Math expressions must be single sexps in the syntax of `Input-mode'.
    The next two points clarify this.

    3a.  Math expressions MUST be enclosed in parentheses, \"\(\" and \"\)\".
         Parentheses are the ONLY way of delimiting math expressions
         which represent atomic coordinates that is acceptable to both
         `Input-mode' and `calc'.

    3b.  Simple math expressions, i.e. numbers or isolated variables,
         can omit the parentheses or not, as you wish.  Fractions do
         not require parentheses so long as the fraction contains no
         whitespace and is relatively simple.  \"1/3\" and \"a/2\" do
         not need parens, \"(1/(a+b))\" does.

    When in doubt, use parens.

4.  Variable names should not use characters that will confuse `calc'.
    For example, \"delta_ti\" is not a valid variable name because \"_\"
    is a math symbol in the syntax of `calc'.  As a general rule, stick
    with alphanumerics.

5.  Any function understood by `calc-eval' can be used in a math
    expression, so if you really need a Bessel's function to describe
    an atom position -- go crazy."
  (interactive)
  (save-excursion
    (cond ((input-evaluation-p)
	   (back-to-indentation)
	   (unless arg
	     (Atoms-read-definitions)
	     (dotimes (n (length Atoms-variables-alist) t)
	       (set (intern (concat "var-" (car (elt Atoms-variables-alist n))))
		    (cdr (elt Atoms-variables-alist n))) ))
	   (forward-char 2)
	   (let (elem (xyz ()) (tag " ") (rest " ") begin sexp space check
		      (eol (save-excursion (end-of-line) (point)))
		      (calc-eval-error "string"))
	     (forward-word 1)
	     (save-excursion
	       (backward-word 1)
	       (setq space (make-string (current-column) ? )))
	     (setq elem (input-this-word)) ; get element symbol

	     (dotimes (x 3 t) ; get coordinates
	       (setq begin (point))
	       (forward-sexp)
	       (if (> (point) eol)
		   (setq xyz (append xyz (list 0)))
		 (setq sexp (buffer-substring-no-properties begin (point))
		 ;;----------------------------------------------------
		 ;; Need to clean up the string because calc under
		 ;; XEmacs 20 cannot grok whitespace within the string.
		 ;; Remove spaces, tabs, leading paren, trailing paren
		 ;;----------------------------------------------------
		       sexp (remove* ?    sexp :test 'char-equal)
		       sexp (remove* ?\t  sexp :test 'char-equal))
		 (if (string= "(" (substring sexp 0 1))
		     (setq sexp (substring sexp 1)))
		 (if (string= ")" (substring sexp -1))
		     (setq sexp (substring sexp 0 -1)))
		 (setq sexp
		       (string-to-number
			(calc-eval (concat "evalv\(" sexp "\)")))
		       xyz (append xyz (list sexp))) ))

	     (forward-word 1) ; get tag
	     (unless (> (point) eol)
	       (setq tag (input-this-word)))

	     (unless (>= (point) eol)
	       (setq begin (point)) ; rest of line
	       (goto-char eol)
	       (setq rest (buffer-substring-no-properties begin (point))))

	     (goto-char eol)
	     (forward-line 1)
	     (save-excursion        ; check next line
	       (end-of-line)
	       (setq eol (point))
	       (back-to-indentation)
	       (forward-word 5) ;  where the tag should be
	       (setq check " ") ;  hmmm....
	       (when (<= (point) eol)
		 (setq check (input-this-word))))
	     (when (and (string= tag check)
			(not (input-comment-p))
			(not (input-blank-p)) )
	       (delete-region (point) eol)
	       (delete-char 1))
	     (insert (format "%s%-2s\t%7.4f\t%7.4f\t%7.4f\t%-10s  %s\n"
			     space elem (elt xyz 0) (elt xyz 1)
			     (elt xyz 2) tag rest)) ))
	  (t
	   (message (concat "This is not an evaluation line.  "
			    "Evaluation lines begin with \"!+\".")))) ))

(defun Atoms-evaluate-buffer ()
  "Evaluate all evaluation lines in the buffer.
See the document string for `Atoms-evaluate-line'."
  (interactive)
  (let ((expr (concat "^[ \t]*"
		      (regexp-quote Atoms-evaluation-comment-string))))
    (Atoms-read-definitions)
    (dotimes (n (length Atoms-variables-alist) t)
	     (set
	      (intern (concat "var-" (car (elt Atoms-variables-alist n))))
	      (cdr (elt Atoms-variables-alist n))) )
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward expr (point-max) t)
	(Atoms-evaluate-line t) ) )
    t))

;; predicate funtions

(defun input-evaluation-p ()
  "Return t if point is in an evaluation line."
  (save-excursion
    (back-to-indentation)
    (looking-at (regexp-quote Atoms-evaluation-comment-string))))

(defun input-definition-p ()
  "Return t if point is in a definition line."
  (save-excursion
    (back-to-indentation)
    (looking-at (regexp-quote Atoms-definition-comment-string))))

(defun input-evaluation-or-definition-p ()
  "Return t if point is in an evaluation or definition line."
  (or (input-evaluation-p) (input-definition-p)))

;; add key bindings and menu entries.  toolbar icons are added
;; conditional on (featurep 'calc) in fuse-toolbar.

(when Atoms-mode-map
  (define-key Atoms-mode-map "\C-c\C-eb"  'Atoms-evaluate-buffer)
  (define-key Atoms-mode-map "\C-c\C-el"  'Atoms-evaluate-line)

  (cond ((string-match "XEmacs" emacs-version)
	 (add-menu-button '("Atoms")
			  ["Evaluate Line"   Atoms-evaluate-line   t])
	 (add-menu-button '("Atoms")
			  ["Evaluate Buffer" Atoms-evaluate-buffer t]) )
	(t
	 (define-key-after
	   (lookup-key Atoms-mode-map [menu-bar Atoms])
	   [evaluate-line] '("Evaluate line" . Atoms-evaluate-line)
	   [Display atoms keywords])
	 (define-key-after
	   (lookup-key Atoms-mode-map [menu-bar Atoms])
	   [evaluate-buffer] '("Evaluate buffer" . Atoms-evaluate-buffer)
	   [evaluate-line])
	 )) )

(provide 'fuse-math)
