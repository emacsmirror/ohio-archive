;; fort-font.el --- A Font Lock setup for Fortran mode.
;; Copyright (C) 1995 Ulrik Dickow.

;; Author: Ulrik Dickow <dickow@nbi.dk> (http://www.nbi.dk/~dickow/)
;; Created: 13-Jan-1995 (as version 0.1)
;; Maintainer: Ulrik Dickow <dickow@nbi.dk> (http://www.nbi.dk/~dickow/)
;; Version: $Id: fort-font.el,v 2.2 1995/09/11 14:46:01 dickow Exp $
;; Keywords: languages faces font-lock fortran
;; See also: fortran.el kumac-font.el html-font.el
;; See also URL: http://www.nbi.dk/~dickow/emacs/

;; This file is neither part of GNU Emacs nor GNU XEmacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; LCD Archive Entry:
;; fort-font|Ulrik Dickow|dickow@nbi.dk|
;; A Font Lock setup for Fortran mode (Emacs <19.29 or any XEmacs).|
;; 11-Sep-1995|2.2|~/misc/fort-font.el.Z|

;;; Commentary:

;; Sets up Fortran mode for some additional support of font-lock-mode.
;; Recommended emacs versions:
;;   * Any XEmacs (current latest is 19.13; it's built-in font-lock is still
;;       confused by quotes in full-line comments, while fort-font is not)
;;   * Emacs <19.29 (19.29's built-in font-lock is better than fort-font's)

;; A reasonable style of writing is assumed (e.g. splitting up words with
;; spaces will not always work).

;; Installation, currently:
;;   1) Put fort-font.el in your load-path and `M-x byte-compile-file' it.
;;   2) Add one of the following lines to your ~/.emacs (without the ";;" in
;;      front), depending on whether you use XEmacs or Emacs:
;;
;;        (require 'fort-font) ; XEmacs
;;        (eval-after-load "fortran" '(require 'fort-font)) ; Emacs 
;;
;; Customize font-lock as you would do for any other major mode.
;;   For XEmacs and Emacs 19.29, default colours are set up automatically.
;;   (For XEmacs you might like to set `font-lock-use-colors' and
;;   `font-lock-use-maximal-decoration' to `t' before the `require'.)
;;   For earlier Emacsen, you have to write a lot yourself (or get it from
;;   somewhere else, e.g. Simon Marshall's face-lock.el in the LCD; see also
;;   http://www.nbi.dk/~dickow/emacs/), for example:
;;   3) To have a colorful display with font lock in any supported major mode,
;;      you can add something like this (Emacs < 19.29):
;;        (copy-face 'bold 'font-lock-function-name-face)
;;        (set-face-foreground 'font-lock-function-name-face "blue")
;;        (setq font-lock-function-name-face 'font-lock-function-name-face)
;;      and similary for `font-lock-string-face' etc. (see font-lock.el).
;;   4) To have font-lock turned on for all fortran-mode buffers do
;;        (add-hook 'fortran-mode-hook '(lambda () (font-lock-mode 1)))

;; Example speed: 125Kb Fortran on a Linux 486/66 took 12 sec. (Emacs+XEmacs);
;;   hilit19 used 20 sec. This doesn't include unhighlighting (hilit19 slow).

;; Bugs: XEmacs 19.13 exhibits an off-by-1 bug with overlapping regexps.
;;       For example, a string with the end quote in column 73 is fontified,
;;       even though it shouldn't be.  This does not happen in Emacs 19.27/28.

;;; Code:

(require 'font-lock) ; This is important for XEmacs (`defconst's everything)

;; Unbalanced quotes in full-line comments make (X)Emacs' font-lock think
;; we have a long multi-line string. So let's handle strings and in-line
;; comments ourselves (with regexps), and thus not use the syntax table.

(defconst fortran-font-lock-keywords-1
 (list
  ;; Highlight full-line comments.  Without the `t', Emacs 19.27/28 is
  ;; slow on the second fontification of a given buffer.  Very strange.
  '("^[c*].*" 0 font-lock-comment-face t)
  ;; Highlight column 73-* comments. A TAB in column 1 marks extended source.
  (list (concat "^[^c*\t]" (make-string 71 ?.) "\\(.*\\)")
	1 'font-lock-comment-face)
  ;; Highlight in-line comments (being careful about !'s in strings).
  '("^[^!'\n]*\\('[^'\n]*'[^!'\n]*\\)*\\(!.*\\)" 2 font-lock-comment-face t)
  ;; Highlight strings.
  '("'[^'\n]*'" . font-lock-string-face)
  ;; Highlight program unit (or entry point) names being defined.
  (list (concat "\\<\\(entry\\|function\\|program\\|"
		"subroutine\\)[ \t]+\\(\\w+\\)")
	2 'font-lock-function-name-face))
 "For consideration as a value of `fortran-font-lock-keywords'.
This does fairly subdued highlighting.")

(defconst fortran-font-lock-keywords-2
  (append
   fortran-font-lock-keywords-1
   (list
;; Produced the following monstrous regexp by using Simon Marshall's
;; make-regexp from the LCD, like this (and wrapped "\\<\\(...\\)\\>"):
;     (make-regexp
;      (let ((simple-types '("character" "byte" "integer" "logical"
;			    "none" "real" "complex"
;			    "double[ \t]*precision" "double[ \t]*complex"))
;	    (structured-types '("structure" "union" "map"))
;	    (other-types '("record" "dimension" "parameter" "common" "save"
;			   "external" "intrinsic" "data" "equivalence")))
;	(append
;	 (mapcar (lambda (x) (concat "implicit[ \t]*" x)) simple-types)
;	 simple-types
;	 (mapcar (lambda (x) (concat "end[ \t]*" x)) structured-types)
;	 structured-types
;	 other-types)))
    (list
     (concat "\\<\\(byte\\|c\\(haracter\\|om\\(mon\\|plex\\)\\)\\|"
	    "d\\(ata\\|imension\\|ouble"
	    "[ \t]*\\(complex\\|precision\\)\\)\\|"
	    "e\\(nd[ \t]*\\(map\\|structure\\|union\\)\\|"
	    "quivalence\\|xternal\\)\\|"
	    "i\\(mplicit[ \t]*\\(byte\\|"
	    "c\\(haracter\\|omplex\\)\\|"
	    "double[ \t]*\\(complex\\|precision\\)\\|"
	    "integer\\|logical\\|none\\|real\\)\\|"
	    "nt\\(eger\\|rinsic\\)\\)\\|"
	    "logical\\|map\\|none\\|parameter\\|re\\(al\\|cord\\)\\|"
	    "s\\(ave\\|tructure\\)\\|union\\)\\>")
     1 'font-lock-type-face)
    ;; Highlight subroutine names being called.
    '("\\<call[ \t]+\\(\\w+\\)" 1 font-lock-doc-string-face)
    ;; Highlight statement labels being defined
    "^[ 0-9][ 0-9][ 0-9][ 0-9][ 0-9]"
    ;; 123456 Highlight a continuation character in standard Fortran 77
    '("^     \\([^ 0]\\)" 1 font-lock-type-face)
    ;; Highlight a continuation character in TAB-formatted line
    '("^\t\\([1-9]\\)" 1 font-lock-type-face)
    ;; Highlight some keywords (incl. referenced labels).
    "\\<\\(do[0-9 \t]*\\|continue\\|end ?do\\|go ?to[ \t]*[0-9]+\\)\\>"
    "\\<\\(if\\|then\\|else\\|elseif\\|end ?if\\|format\\)\\>"
    ;; Highlight goto-like `err=label' & `end=label' in read/write statements
    '(",[ \t]*\\(\\(err\\|end\\)[ \t]*=[ \t]*[0-9]+\\)"
      1 font-lock-keyword-face)
    ;; Highlight these main keywords specially, like hilit19 does.
    (list (concat "\\<\\(en\\(d\\|try\\)\\|function\\|include\\|program\\|"
		  "return[0-9 \t]*\\|s\\(top\\|ubroutine\\)\\)\\>")
	  1 'font-lock-doc-string-face)))
  "For consideration as a value of `fortran-font-lock-keywords'.
This does a lot more highlighting.")

;; Default to the gaudier variety :-)
(defvar fortran-font-lock-keywords fortran-font-lock-keywords-2
  "Additional expressions to highlight in Fortran mode.")

(cond ((string-match "XEmacs\\|Lucid" (emacs-version))
       ;; XEmacs no longer (19.13) understands `font-lock-no-comments'.
       ;; Thus we have to do the following hack, unless we modify the fortran
       ;; syntax table permanently (remove all comment and string syntax from
       ;; it).
       (add-hook 'font-lock-mode-hook
		 (function (lambda ()
			     (if (eq major-mode 'fortran-mode)
				 (setq font-lock-use-syntax-tables nil)))))
       (setq fortran-font-lock-keywords ; Beat XEmacs' `defconst'.
	     (if (and (boundp 'font-lock-use-maximal-decoration)
		      (not font-lock-use-maximal-decoration))
		 fortran-font-lock-keywords-1
	       fortran-font-lock-keywords-2)))
       (t
;; It shouldn't matter whether this hook is executed before or after font-lock.
;; Fortunately, Emacs' font-lock.el is very friendly in this respect :-)
;; We must be equally friendly and make sure we don't make global defaults.
	(add-hook 'fortran-mode-hook
		  '(lambda ()
		     (make-local-variable 'font-lock-keywords-case-fold-search)
		     (make-local-variable 'font-lock-keywords)
		     (make-local-variable 'font-lock-no-comments)
		     (setq font-lock-keywords-case-fold-search t)
		     (setq font-lock-keywords fortran-font-lock-keywords)
		     (setq font-lock-no-comments t)))))

(provide 'fort-font)

;;; fort-font.el ends here
